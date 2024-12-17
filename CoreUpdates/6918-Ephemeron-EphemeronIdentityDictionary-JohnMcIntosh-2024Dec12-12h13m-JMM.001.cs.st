'From Cuis7.3 [latest update: #6913] on 12 December 2024 at 12:16:10 pm'!
!classDefinition: #EphemeronIdentityDictionary category: #'System-Finalization'!
IdentityDictionary subclass: #EphemeronIdentityDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Finalization'!

!EphemeronIdentityDictionary commentStamp: '<historical>' prior: 0!
I am an identity dictionary holding only weakly on my keys.
When one of my keys is garbage collected, the key->value association is removed from the dictionary.

Internally I use Ephemeron objects that know myself as their container.
Ephemeron are implemented as ephemerons and get mourned when their key is only known from itself.
On mourn, the association asks the container to remove itself.!

!classDefinition: #Ephemeron category: #'System-Finalization'!
Association ephemeronSubclass: #Ephemeron
	instanceVariableNames: 'container'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Finalization'!

!Ephemeron commentStamp: '<historical>' prior: 0!
An Ephemeron is an association known to the garbage collection system, allowing it to function as a pre-mortem finalizer.

An Ephemeron is intended for uses such as associating an object's dependents with an object without preventing garbage collection.

Consider a traditional implementation of dependents in non-Model classes.  There is a Dictionary in Object, DependentsFields, into which objects wishing to have dependents are entered as keys, with the value being the sequence of their dependents.  Since a key's dependents (if they are like views/morphs, etc in MVC) will refer directly back to the key (e.g. in their model inst var etc), the key remains srongly referenced; there is no way to use weak collections in DependentsFields to allow the cycle of an object and its dependents to be collected.  If DependentsFields were to use a WeakArray to hold the associations from objects to their dependents then those associations, and the dependencies they record, would simply be lost since the only reference to the associations is from DependentsFields.

Ephemeron differs from a normal association in that it is known to the garbage collector and it is involved in tracing.  First, note that an Ephemeron is a *strong* referrer.  The objects it refers to cannot be garbage collected.  It is not weak.  But it is able to discover when it is the *only* reference to an object.  To be accurate, an Ephemeron is notified by the collector when its key is only referenced from the transitive closure of references from ephemerons.  i.e. when an ephemeron is notified we know that there are no reference paths to the ephemeron's key other than through ephemerons; the ephemeron's key is not otherwise reachable from the roots.

Ephemerons are notified by the garage collector placing them in a queue and signalling a semaphore for each element in the queue.  An image level process (the extended finalization process) extracts them from the queue and sends mourn to each ephemeron (since their keys are effectively dead).  What an Ephemeron does in response to the notification is programmable (one can add subclasses of Ephemeron).  But the default behaviour is to send finalize to the key, and then to remove itself from the dictionary it is in, allowing it and the transitive closure of objects reachable from it, to be collected in a subsequent garbage collection.

Implementation: both in scavenging, and in scan-mark, if an ephemeron is encountered its key is examined.  If the key is reachable from the roots (has already been scavenged, or is already marked), then the ephemeron marked and treated as an ordinary object. If the key is not yet known to be reachable the ephemeron is held in an internal queue of maybe triggerable ephemerons, and its objects are not traced.

At the end of the initial scavenge or scan-mark phase, this queue of triggerable ephemerons is examined.  All ephemerons in the list whose key is reachable are traced, and removed from the list.  i.e. what has happened was that their key was found reachable form the roots after they were added in the list (the garbage collector traces the object graph in an arbitrary order, typically breadth first in the scavenger, depth-dirst in teh scan-mark).  This then leaves the list populated only with ephemerons whose keys are as yet untraced, and hence only referenced from the ephemerons in the triggerable ephemeron queue, which now becomes the triggered ephemeron queue.  All these ephemerons are placed in the finalization queue for processing in the image above, and all objects reachable from these ephemerons are traced (scavenged, marked).  This tracing phase may encounter new potentially triggerable ephemerons which will be added to the triggerable ephemeron queue (not likely in practice, but essential for sound semantics).  So the triggering phase continues until the system reaches a fixed point with an empty triggerable ephemeron queue.

Implications and advantages:
Because ephemerons do not allow their object to be collected, they can be, and are, used to implement pre-mortem finalization.  So e.g. a file can flush its buffers and then close its file descriptor before being collected (which may also imply that the system runs the garbage collector *before* snapshotting, not as part of the snapshot primitive).  Ephemerons are conceptually more simple than WeakKeyDictionary et al, since they are about reference paths, not merely the existence of strong references.  They accurately identify when an object is no longer reachable from the roots, no matter how many ephemerons are attached to a specific object for whatever purpose.  Note that the back reference from a dependent to an object renders a weak key dictionary useless in enabling an isolated cycle to be collected since the back reference is string, and keeps the reference from the weak key alive.

History: Ephemerons are like guardians.  They were invented by George Bosworth in the early '90's, to provide pre-mortem finalization and to solve the problem of DependentsFields retaining garbage.

Instance Variables
	container 	<Dictionary|Set> - the container in which the Ephemeron resides.
!


!EphemeronIdentityDictionary methodsFor: 'accessing' stamp: 'JMM 12/6/2024 10:37:01'!
associationClass
	^Ephemeron! !

!EphemeronIdentityDictionary methodsFor: 'adding' stamp: 'JMM 12/12/2024 10:05:22'!
add: anAssociation
	
	"Installs a copy of the association in myself.
	I cannot add the association because they are not shareable between weak key dictionaries, it needs myself as container.
	Moreover, I can only contain Ephemerons, and not other kinds of Association."
	self at: anAssociation key put: anAssociation value.

	^ anAssociation! !

!EphemeronIdentityDictionary methodsFor: 'private' stamp: 'JMM 12/2/2024 14:47:36'!
atNewIndex: index put: anEphemeron

	anEphemeron container: self.
	^super atNewIndex: index put: anEphemeron! !

!EphemeronIdentityDictionary methodsFor: 'copying' stamp: 'JMM 12/12/2024 10:02:44'!
postCopy
	"Must copy the associations and tell them I (the copy) am their new container"

	array := array collect: [ :assoc |
		         assoc ifNotNil: [
			         assoc copy
				         container: self ] ]! !

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'JMM 12/2/2024 15:19:05'!
finalizeEphemeron: anEphemeron

	"Finalize the ephemeron finalizer and remove it from myself.
	Avoid errors during the finalization to let the finalization process proceed"
	
	self removeKey: anEphemeron key ifAbsent: [].
! !


!Ephemeron methodsFor: 'accessing' stamp: 'eem 10/1/2020 12:31'!
container
	"Answer the Dictionary containing the receiver, if any."
	^container! !

!Ephemeron methodsFor: 'accessing' stamp: 'eem 10/1/2020 12:33'!
container: aDictionarySetOrNil
	"Set the Dictionary containing the receiver, or nil it."
	container := aDictionarySetOrNil! !

!Ephemeron methodsFor: 'testing' stamp: 'eem 9/1/2015 10:12'!
isEphemeron
	^true! !

!Ephemeron methodsFor: 'mourning' stamp: 'JMM 12/9/2024 10:28:28'!
mourn
	"Triggered ephemerons get sent mourn by EphemeronsFinalizationProcess class's finalizationProcess
	 (via mournLoopWith:) when the only reference(s) to an ephemeron's key is through
	 one or more ephemerons. i.e. the key is not reachable from the roots of the system,
	 only through ephemerons.  So ephemerons get sent mourn when their key would
	 otherwise have been garbage collected.  Respond to the information that the key
	 would have been garbage collected other than through ephemeron references by
	 sending finalize to the key."
	container finalizeEphemeron: self.! !

