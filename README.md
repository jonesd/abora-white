# Abora-White

Abora-White is an implementation of the Abora server written in Java
and based on the Udanax-Gold source.

[![Build Status](https://travis-ci.org/jonesd/abora-white.png?branch=master)](https://travis-ci.org/jonesd/abora-white)

**Note:** This project is from 2003 and there has been little work on it since then other
than to migrate it to GitHub.

David Jones david@dgjones.info 


## Project Summary

The process of writing Abora-White is to start from a clean project
then iteratively:

- Port a small portion of Udanax-Gold - from my Java Abora-Gold auto-translated version
- Convert to a Java friendly naming, programming idioms and use of existing Java features and libraries
- Write JUnit test to help me understand the implementation and check the port
- Write JavaDoc and summary documentation


## Building and Testing

Currently there is not much to look at, but you can build and run
tests on the code if you have Java + Maven installed.

    mvn clean install


## Porting from Udanax-Gold

In porting Udanax-Gold from Smalltalk to the Java based
Abora-White a number of issues have come up. For example Smalltalk
integers are efficiently encoded as object references within 30
bits or so of size and will transparently expand to large
integer objects beyond that size, whereas Java int's will not
expand beyond their max values, and BigIntegers are second-class
citizens at the syntax level. So what should the Java conversion
do? The purpose of this page is to document these problems and
the final outcome I have chosen for Abora-White.

> Dean Tribble kindly emailed in
> a few comments on the contents of this page. I have included
> them below in the marked lines. Thanks Dean.

**Note:** This analysis of the capabilities of Java is from 2003 and
so is well behind the current state of Java and its support for Generics
and other language features.

### BooleanVar

This was simply replaced with the Java `boolean`
primitive data type.

### IntegerVar

The Smalltalk native support for Integers is very powerful. For
small numbers (&lt; 29(ish) bits) a compact unboxed in-representation form
is used to minimise performance overhead. If an integer larger
than the initial range is needed then the system automatically
changes to a boxed  Integer object format - transparently to the
application user. The boxed and unboxed versions provide the
same protocol, and both support C-like operators such as +. The
only weirdness is that there is no operator precedence rules,
other than computations start from the left.

> These are intended as 32-bit integers. In the
> server we were not anticipating using infinite precision
> Integers.
>
> Note that the translator to X++ did manage
> the transition to C++ precedence correctly.</p>

The X++ version of Udanax-Gold seems to support just 32-bit
integers, which is a surprise. I guess this is either a
misunderstanding on my part, or perhaps unlimited precision
support for a pending feature? C++ allows automatic conversions between int and
IntegerVar class representations.

> The 'Var' suffix meant allocated on the stack as opposed to the heap.
> IntegerVar was defined so that we could control alignment, operators,
> additional type checking, etc., but can be thought of as a 32 bit int.

The Java native support for integers is split between primitive
data types of various bit sizes, and the BigInteger class which
enables unlimited precision integers. There is no automatic
promotion from primitives to BigInteger. Additionally arithmetic
operators are supported for the primitive data types, but not
for BigIntegers.

> I recommend mapping to int in Java.

Assuming Udanax-Gold IntegerVars are required to support
unlimited procession, I have temporarily gone for a wrapped
version of BigInteger. The downsides are performance hits for
small and large numbers, and no arithmetic operator support. The
worst of both worlds!

### Java Primitive Data Types

The Java primitive data types don't completely match those of
X++.

All Java primitives are signed, except for the
`char` type, this contrasts to X++ where there is full
control between signed and unsigned. This is particularly
significant for the UInt32 type which is often used.

> Mostly UInt32 was used for indices, for which negative numbers never
made sense.  They were also used for hashes.  These could almost all
be translated as int and it would be fine.

The second case has to do with Strings; Java string characters
are unsigned 16-bit Unicode, default X++ string characters are unsigned 8-bit.

### Exceptions

Udanax-Gold makes extensive use of Exceptions. I believe that on
the Smalltalk side, the instance based exception mechanism of
the Smalltalk language is used. On the X++ side I think a custom
exception mechanism was implemented - possibly done before C++
exceptions were added to the C++ specification.

> I'm pretty sure they map easily to instances of type-based exceptions,
so the mapping to Java should be straightforward.

Java supports a reasonable class based exception mechanism. A
facility beyond the Udanax-Gold exceptions is the support for
declaring the types of exceptions thrown by a method.

The initial take is to follow the instance style, and I have
added a `org.abora.white.exception.AboraRuntimeException`,
together with a whole bunch of constants extracted from the
exception use in Udanax-Gold. As the name suggests, this is
implemented as a Java RuntimeException so that it does not have
to be included in a methods definition. It is too early to try
and properly define the relevant throws for each method.

The pattern for throwing one of these exceptions is like the
following:
			
    throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);

Over time I would like to move to a class based approach, and
additionally make as much use of the Java built-in exceptions
where possible.

### Using Steppers

When iterating with a Stepper, the Smalltalk version of UG
naturally uses Smalltalk BlockClosures to pass in the code that
will be executed for every step. See the `#forEach` and
`#forPositions` method for example implementation.

<pre>
{void} forEach: fn {BlockClosure} 
	[| elem {Heaper} |
	[(elem _ self fetch) ~~ NULL]
		whileTrue:
			[fn value: elem.
			self step]]
		valueNowOrOnUnwindDo: [self destroy]!
</pre>

The X++ translation does not support BlockClosures, so instead
makes use of the preprocessor define to macros to expand FOR_EACH, or
whatever, out to the contents of the forEach method for each use
of it. So there is no forEach method in the X++ source.

Java has a comparable feature to Smalltalks BlockClosure and so
could implement and use forEach methods. This would 
typically be accomplished through the use of Anonymous inner classes
implementing in this case something like a
Niladic/Monadic/DuoadicValuable interfaces as needed.
Unfortunately this technique is often frowned upon in the Java
community as not matching the standard Java programming style, plus
the implementation overhead of an extra (hidden) class for each
**use** or call.

Java has no built-in preprocessor support, so the
behind-the-covers code expansion of the FOR_EACH macro is not
possible.

Initially I have decided, again, to follow the worst possible
route and effectively hand add the contents of the forEach
method to each use of it. I will review this approach later in
the project.

As an example of the Java code for each equivalent forEach call:

<pre>
TableStepper stepper = myArray.stepper();
	try {
		Heaper e;
		while ((e = (Heaper) stepper.fetch()) != null) {
			newArray.atIntStore((myDsp.ofInt(stepper.index())), e);
			stepper.step();
		}
	} finally {
		stepper.destroy();
	}
</pre>

> As I recall we tried to make sure that it could be translated as a
for-loop.  That would be a little more readable.

...and the original Smalltalk source:

<pre>
(s _ myArray stepper) forEach: [ :e {Heaper} |
	newArray atInt: (myDsp ofInt: s index) store: e].
</pre>

### Object Birth

Object creation is usually started by calling a static factory
method, often named <tt>make</tt>. This can be mapped generally
to Java. The only complications are that a method defined in a
subclass whose signature matches that in a superclass can not
change the return type of the super definition.

> Yes.  Exposing the allocation behavior of a class when you want an
> instance of it prevents opportunities for caching, refactoring, etc.
> so we tried to use factory methods consistently.
>
> Yup, they definitely will need some renaming for Java.

I have used the Java constructors in place of the create
methods. This generally works out, except that Constructors have
a number of 'strange' properties in Java. Constructors are not
inherited, so often I have had to add intervening constructor
definitions which simply call a super constructor. Following the
general factor pattern, the constructors are made protected.

> Exactly the right thing.  Part of the reason for the generic naming of
create methods is that they were supposed to map to constructors.

Some open issues in this area are with the area of the
`new.Become`, and possibly similar methods. Java has not
capability that matches the Smalltalk become feature. The become
feature is mapped into two different ways depending on the
Smalltalk implementation, either one-way or two-way become. A
one-way become will change all objects referencing object A to
now reference object B. The two-way become will change all
object referencing object A to now reference object B **and**
for all objects referencing object B to now reference object A.

> We did not use two-way become except to emulate behavior we could
accomplish in C++ some other way.  We did use one-way become for
converting stubs into objects during unmarshalling.  In Java, you
would need to use Proxies and maintain the layer of forwarding, I
think (or not use lazy unmarshalling).

### Object Death

Both Smalltalk and Java have Garbage Collection built in. Java
additionally supports Weak references and finalization support.
Newer Smalltalk contain similar features, and it is assumed that
the Udanax-Gold Smalltalk included these as well.

> We used the post-mortem finalization support from ObjectWorks.  That's
where it was invented, after all :-)  For various reasons, WeakArrays
are much better than individual weak references, but you can still do
everything with individual weak references.

C++ doesn't support Garbage Collection in the default
implementation, and it is assumed that X++ included some kind of
Garbage Collection system.

> Yes.  That code should be available somewhere.  It was a pretty
impressive GC system.

To be researched; is destruct/destroyed only relevant to X++ or
does it make sense for Smalltalk and Java implementation,
distributed and file based garbage collection support.

> My recollection is that it was only for X++.  We might have used the
distinction elsewhere, but we tried not to.  One of the last things we
did was move the X++ GC to use post-mortem finalization (and not
destructors).

### Equality and Hashing

To be researched...

### Casting

The `castInto` mechanism is quite extensively used. For
example:
			
<pre>
other
	cast: AndFilter into: [:af |
		^af subFilters isEqual: self subFilters]
	others: [^false].
</pre>

This is quite simply hand translated into the following using
standard Java instanceof operator.
			
<pre>
if (other instanceof AndFilter) {
	AndFilter af = (AndFilter) other;
	return af.subFilters().isEqual(subFilters());
} else {
	return false;
}
</pre>

One case to watch out for is if no <tt>others</tt> section is
defined. Reading the X++ notes for the comparable
BEGIN_CHOOSE/BEGIN_KIND/etc feature one sees that in this case
an exception should be thrown, rather than quietly falling
through. See `choosex.hxx`.

### Class Instance Variables

Smalltalk supports both Class Variables and Class Instance
Variables. Class Variables effectively match the static fields
common in C++ and Java. Such a variable is declared in one class,
and a single value is associated with it that can be shared by all
subclasses. Class Instance Variables are again declared once in
association with a class, but the difference is that every
subclass has its own value bound to the variable.

Java does not natively support class instance variables, but it
is relatively straightforward to implement by following the X++
approach. Declare and define a private static variable in the
original host of the class instance variable and all its
subclasses. The static modifier will force each class to have its
own variable and code will bind to the local classes version, and
the private access will ensure that code cant use a superclass
version instead. The only downside is the duplicate definitions
when it comes to code maintenance.

### Category

Udanax-Gold appears to use instances of its Category class to
support runtime class information. Smalltalk and Java both
include built-in class objects that are available in a deployed
runtime application. I assume that C++ of the time didn't,
together with alternative client languages, which prompted the
creation of the Category class.

> Category was part of Smalltalk.  We stuffed information there that
would normally be inline in a Java or C++ file (like instance variable
types).  In java, there should be no need for them.

For the moment the Java Class objects are being used in place
of Categories. In the future the Category class my need to be
re-added as more is learnt about its use.

See: `../udanax-gold2java/abora-gold/api/org/abora/gold/xpp/basic/Category.html`

### Potential Bugs

During the course of porting from Udanax-Gold a number of
potential bugs have been found. This happened by either close
reading of the code or by creating JUnit tests for the code. I
felt it was useful to list my discoveries here; to help me later
when I might run into problems with code relying on this, help
others who may attempt a port of Udanax-Gold to other systems, and
for the active readers amongst you to double check my claims and
hopefully show me that I was wrong.

I have listed these items as *potential* bugs as I could
well be mistaken about the original purpose of the code,
overlooking details of the Smalltalk/X++ low level implementation
or quite possible gross user incompetence on my side.

As a closing statement, I hope nobody takes offence at
producing such a list, I clearly have great respect for
Udanax-Gold to even consider spending this amount of time on it,
and we all know that no reasonable application can be defect
free.

- PrimIntegerArray:indexPastInteger - when nth is < 0, wont
 find match on index=0 (should have result >= 0)</li>

- Pair:isEqual - throws exception if you use obsolete pairs
				with alternative null values: <tt>(Pair.pairWithNulls(aHeaper,
					null)).isEqual(Pair.pairWithNulls(null, aHeaper))</tt></li>

- MuSet &amp; ImmuSet use incompatible contentsHash mechanisms.
			ActualHashSet adds together element hashes, whereas ImmuSets
			(inherited from ScruSet) bitXor together element hashes. I
			assume this is an oversight rather than a designed, but
			undocumented, mechanism to force Mu/ImmuSet with the same
			elements to produce different contentHashes.

- IntegerRegion:below was actually a duplicate of the above
					method.

> The scary thing is that I remember this bug, so your code may be a
slightly-earlier-than-final snapshot.


## Infrastructure

The core of the software is a generic server that supports
documents/data structures, sharing of and linking between,
versioning, notification of various changes, access control, and
communication with other servers.

The server provides a service at a relatively low level and
generic level. There is an expectation that it will be very rare
for developers to have to modify the server.

The server is hopefully going to be based on a translated
version of the open-source Udanax-Gold code converted into Java by
a combination of one-off automated-translation followed by hand
tweaking. If this doesn't prove feasible then a coded from scratch
implementation will be considered as an extension of my earlier
dolphin prototype work, though this will have to have a severely
reduced set of features compared with Udanax-Gold. The java
version may be hosted in JBoss for transaction/persistence support
though that is open to change.

A network of servers should eventually be supported. A client
would typically communicate with a single server, and that server
in turn would communicate with other servers to retrieve external
documents. A user could run a server on their own machine to
support working while disconnected from the server network.

Beyond the server are a number of front-end/client applications.

Front-Ends communicate with the server using a platform and
language neutral client API. This can be local or network access.
Messages can be in a compact binary format for runtime
performance, or an XML version for debugging and more open
access.

The client API appears to be pretty complicated and most
naturally supported by OOP languages, though at a low-level the
existing functionality appears to fall back to a single list of C
like function calls.

One type of front-end could be a webserver gateway. This would
enable a website to use the abora back-end to present HTML pages.
A basic version of this could support viewing and simple editing.
Application developers could then build on this to provide
application specific features, formatting, etc.

Thick client would enable more sophisticated editing, and could
enable existing stand-alone applications to use the server by
supporting the client API. A basic version could be supplied that
supports all the raw features and perhaps support for notes/web
links/diary/etc. Custom versions could extend from this.

Simple command-line driven front-ends could provide some
straight forward access for automated manipulation and weak
integration with existing applications. This might support
importing of entire documents, retrieving entire documents at a
content level, and perhaps back-up support.

Indexing of words/terms such as Google provides is not part of
the core server. Even with the sophisticated linking supported by
the server, indexing will be a critical feature. Indexing
front-ends would be used to trawl the server documents in a
similar way to HTML indexing. I don't know if these index
structures would be stored in turn in the server, and if access to
them could be direct or via the indexer front-end.

## Programming Patterns

### Object Creation

Object creation is requested using static factory methods
rather than directly calling constructors. This is an often used
pattern that has a few benefits; factory methods can have names,
they gain flexibility by being able to return subclasses of the
declared type and also they can re-use existing objects if
required.

The default name for the factory methods is <tt>make</tt>
though there are some more descriptively named cases as well.</p>

Some simple examples of its usage:

<pre>
IntegerValue nine = IntegerValue.make(9);
IntegerValue zero = IntegerValue.zero();
</pre>

Constructors are obviously still used under the covers, and are
often marked as `protected` to discourage their direct
calling.

TODO: References, why the name make, are there any idoms for
naming these factories beyond make, problems with return
inheritance, rehydrating with the Rcvr constructor.

### Steppers

Steppers are an implementation of the iterator pattern, that is
present in Java as the Enumeration and Iterator interfaces.

To be continued...


## Contact

david@dgjones.info

https://github.com/jonesd/abora-white

## Copyright and licence

Abora White is Copyright 2003, 2014 David G Jones

Licensed under MIT X-11.

Substantial portions of the code are from the Udanax-Gold project and are Copyright 1979-1999 Udanax.com and licensed under MIT-X11. 
