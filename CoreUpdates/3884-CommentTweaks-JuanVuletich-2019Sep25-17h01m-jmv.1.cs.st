'From Cuis 5.0 [latest update: #3883] on 25 September 2019 at 5:03:34 pm'!

!DiskProxy commentStamp: '<historical>' prior: 0!
A DiskProxy is an externalized form of an object to write on a
DataStream. It contains a "constructor" message to regenerate
the object, in context, when sent a comeFullyUpOnReload: message
(i.e. "internalize").

We are now using DiskProxy for shared system objects like StrikeFonts.

The idea is to define, for each kind of object that needs special
externalization, a class method that will internalize the object by
reconstructing it from its defining state. We call this a
"constructor" method. Then externalize such an object as a frozen
message that invokes this method--a DiskProxy.

(Here is the old comment:
Constructing a new object is good for any object that (1) can not be
externalized simply by snapshotting and reloading its instance
variables (like a CompiledMethod or a Picture), or (2) wants to be
free to evolve its internal representation without making stored
instances obsolete (and dangerous). Snapshotting and reloading an
object"s instance variables is a dangerous breach of encapsulation.

The internal structure of the class is then free to evolve. All
externalized instances will be useful as long as the
constructor methods are maintained with the same semantics.

There may be several constructor methods for a particular class. This
is useful for (1) instances with characteristically different
defining state, and (2) newer, evolved forms of an object and its
constructors, with the old constructor methods kept around so old
data can still be properly loaded.)

Create one like this example from class Picture

    DiskProxy global: #Picture
            selector: #fromByteArray:
                args: (Array with: self storage asByteArray)

* See also subclass DiskProxyQ that will construct an object in
the above manner and then send it a sequence of messages. This may save
creating a wide variety of constructor methods. It is also useful because
the newly read-in DiskProxyQ can catch messages like #objectContainedIn:
(via #doesNotUnderstand:) and add them to the queue of messages to
send to the new object.

* We may also want a subclass of DiskProxy that evaluates a string
expression to compute the receiver of the constructor message.

My instance variables:
* globalObjectName -- the Symbol name of a global object in the
    System dictionary (usually a class).
* constructorSelector -- the constructor message selector Symbol to
    send to the global object (perform:withArguments:), typically a
    variation on newFrom:.
* constructorArgs -- the Array of arguments to pass in the
    constructor message.

-- 11/9/92 Jerry Morrison
!


!VariableNode methodsFor: 'testing' stamp: 'jmv 9/24/2019 12:51:03'!
isSuperPseudoVariable
	"Answer if this ParseNode represents the 'super' pseudo-variable."

	^ key = 'super' or: [name = '{{super}}']! !

