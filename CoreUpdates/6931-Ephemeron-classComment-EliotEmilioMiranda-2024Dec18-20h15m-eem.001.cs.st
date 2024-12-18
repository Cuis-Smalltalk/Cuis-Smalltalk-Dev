'From Cuis7.3 [latest update: #6922] on 18 December 2024 at 8:18:11 pm'!

!Ephemeron commentStamp: 'eem 12/18/2024 10:26:19' prior: 0!
An Ephemeron is an association known to the garbage collection system, allowing it to function as a "pre-mortem" finalizer, as opposed to "post-mortem" finalization.

Weak array based finalization schemes work by having a WeakArray of objects whose collection will be observed by a weak array element being nilled, and a parallel array of corresponding copies of the collectable objects. The corresponding object copy is finalized some time after the weakly referenced object is collected. This is known as post-mortem finalization and suffers the problem that the copy is finalized, not the original object. This requires effort to synchronize the original and the copy while avoiding strong references to the original, preventing its collection.

Unlike the weak scheme, ephemerons notice when their key is only referenced through ephemerons, consequenctly they can send #finalize to their key before their key is collected but when it is found to be collectable. So this finalizes the actual object with no need to maintain a copy, and is known as pre-mortem finalization.

Details:

An Ephemeron is intended for uses such as associating an object's dependents with an object without preventing garbage collection.

Consider a traditional implementation of a property table (for example, dependents in MVC).  There is a Dictionary in ActiveModel, ActionMaps, into which objects wishing to have dependents are entered as keys, with the value being the sequence of their dependents.  Since a key's dependents (if they are like views/morphs, etc in MVC) will refer directly back to the key (e.g. in their model inst var etc), the key remains srongly referenced; there is no way to use weak collections in DependentsFields to allow the cycle of an object and its dependents to be collected.  If ActionMaps were to use a WeakArray to hold the associations from objects to their dependents then those associations, and the dependencies they record, would simply be lost since the only reference to the associations is from ActionMaps.

Ephemeron differs from a normal association in that it is known to the garbage collector and it is involved in tracing.  First, note that an Ephemeron is a *strong* referrer.  The objects it refers to cannot be garbage collected.  It is not weak.  But it is able to discover when it is the *only* reference to an object.  To be accurate, an Ephemeron is notified by the collector when its key is only referenced from the transitive closure of references from ephemerons.  i.e. when an ephemeron is notified we know that there are no reference paths to the ephemeron's key other than through ephemerons; the ephemeron's key is not otherwise reachable from the roots.

Ephemerons are notified by the garage collector placing them in a queue and signalling a semaphore for each element in the queue.  An image level process (the extended finalization process) extracts them from the queue and sends mourn to each ephemeron (since their keys are effectively dead).  What an Ephemeron does in response to the notification is programmable (one can add subclasses of Ephemeron).  But the default behaviour is to send #finalize to the key, and then to remove itself from the dictionary it is in, allowing it and the transitive closure of objects reachable from it, to be collected in a subsequent garbage collection.

Implementation:

Both in scavenging, and in mark-sweep, if an ephemeron is encountered its key is examined.  If the key is reachable from the roots (has already been scavenged, or is already marked), then the ephemeron marked and treated as an ordinary object. If the key is not yet known to be reachable the ephemeron is held in an internal set of maybe triggerable ephemerons, and its objects are not traced.

At the end of the initial scavenge or mark-sweep phase, this set of triggerable ephemerons is examined.  All ephemerons in the set whose key is reachable are traced, and removed from the set.  i.e. what has happened was that their key was found reachable from the roots after they were added in set list (the garbage collector traces the object graph in an arbitrary order, typically breadth first in scavenging, depth-first in mark-sweep).  The set is then populated only with ephemerons whose keys are as yet untraced, and hence only referenced from the ephemerons in the set..  All these ephemerons are added to the finalization queue for processing in the image above, and all objects reachable from these ephemerons are traced (scavenged or marked).  This tracing phase may encounter new potentially triggerable ephemerons which will be added to the set (not likely in practice, but essential for sound semantics).  So the triggering phase continues until the system reaches a fixed point with an empty triggerable ephemeron set.

Implications and advantages:

Because ephemerons do not allow their object to be collected, they can be, and are, used to implement pre-mortem finalization.  So e.g. a file can flush its buffers and then close its file descriptor before being collected (which may also imply that the system runs the garbage collector *before* snapshotting, not as part of the snapshot primitive).  Ephemerons are conceptually more simple than WeakKeyDictionary et al, since they are about reference paths, not merely the existence of strong references.  They accurately identify when an object is no longer reachable from the roots, no matter how many ephemerons are attached to a specific object for whatever purpose.  Note that the back reference from a dependent to an object renders a weak key dictionary useless in enabling an isolated cycle to be collected since the back reference is string, and keeps the reference from the weak key alive.

History:

Ephemerons are like guardians.  They were invented by George Bosworth in the early '90's, to provide pre-mortem finalization and to solve the problem of DependentsFields retaining garbage in MVC.

Instance Variables
	container 	<Dictionary|Set> - the container in which the Ephemeron resides.!

