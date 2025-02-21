# A semantics for Ephemerons (& Weak Referrers).


## Semantics

A Smalltalk system is composed of a live object graph, which is the transitive closure of the objects reachable from the roots. The roots are effectively the Smalltalk dictionary and the current computation (the ```thisContext``` of ```Process activeProcess```). The job of the garbage collector is to reclaim objects that fall outside of the object graph.

By reachability we mean that objects refer to each other through variables. A variable, be it an instance variable or a temporary variable, is a reference to some object. A system containing ephemerons and weak objects has four kinds of references:
- normal, a.k.a. "strong", references through variables
- references from ephemerons through their key instance variables (the first instance variable of each ephemeron)
- references from ephemerons through their other instance variables
- weak references (which are the indexed variables of weak arrays, whose named instance variables - if any - are normal, strong references)

In defining the semantics of ephemerons we are interested in two different transitive closures of the object graph accessible from the roots.

To define the first transitive closure the four categories of reference can be reduced to two, all references other than weak references, and weak references. The first category are strong references and include both kinds of reference from ephemerons. The transitive closure of reachable objects from the system roots through strong references derives the live set of objects. In any partial garbage collection the system may reclaim a subset of objects lying outside this set. In a full garbage collection, the system must reclaim all objects lying outside this set. In any garbage collection, partial or full, the system must nil indexed variables of weak objects referring to objects lying outside the set of live objects.

The second transitive closure is that obtained by following all strong references, but not following any reference (of either kind) from ephemerons. Note that this will include ephemerons accessible from the roots, but not ephemerons only accessible via other ephemerons[^1]. The set difference of the first and second transitive closures are those objects reachable only via ephemerons. Within this set there is a subset of the objects referred to only by the key of any ephemeron within the first set (the set of all live objects). This subset is the set of keys reachable only through ephemerons. We shall call it the triggered keys set.

[^1]: Note that any ephemerons not reachable from the roots will be collected, just like any other objects.

In any partial garbage collection the system may trigger[^2] some subset of the ephemerons whose keys are in the triggered keys set. In a full garbage collection the system should trigger all the ephemerons whose keys are in the triggered keys set, but may not if the number of these ephemerons is very large. However, repeated garbage collections must trigger all ephemerons whose keys are in the triggered keys set. When an ephemeron is triggered it loses its "ephemeron-ness" and becomes an ordinary object. In subsequent garbage collections its key reference is no longer treated specially.

[^2]: "trigger" means "to queue the Ephemeron for finalization"

Triggered ephemerons are put in a queue, "the mourn queue", and the system takes triggered ephemerons out of the queue for finalization.

## Usage 

Informally, the above semantics mean that ephemerons detect when objects are about to be garbage collected. Objects in the triggered keys set would be garbage collected were it not for the references from ephemerons. So attaching an ephemeron to an object (by making an object a key of an ephemeron), and putting the ephemeron in some collection reachable from the roots, will cause the ephemeron to be triggered when that object is only reachable from the keys of ephemerons. If there is only one ephemeron attached to a specific object then that ephemeron will be triggered as soon as the garbage collector detects that the object is only so reachable. If there are multiple such ephemerons, each will be triggered.

This is very useful. It allows us to finalize objects by removing ephemerons from the ephemeron queue and sending them the mourn message. In response, the ephemeron sends finalize to its key object, and removes itself from its collection. Provided that finalization does not cause the object to become reachable from the roots then in a subsequent garbage collection both the ephemeron and the finalized object will be collected. Hence ephemerons allow us to finalize objects before they are collected. An example would be a file which finalizes by flushing its contents to the file system and closing its file handle.

This should be contrasted with the "traditional" finalization scheme in Smalltalk using weak references. Here, the garbage collector sets to nil any indeable slot in a weak array referring to a reclaimed object. The system is notified when this happens and then sends finalize to any weak arrays which have lost one reference or more. For each weak array a parallel strong array refers to corresponding objects that are finalized. So for each open file there exists a copy of the file with the same file handle. When the file is collected the weak array referring to it will have that slot nilled, be sent finalize, in response locate the corresponding file, and send it finalize. But in this case all the copy of the file can do is close the file handle.  Unless the corresponding file is updated constantly it cannot flush its contents, because it is not in sync with the collected file.

## Spur Implementation

The Spur memory manager keeps all objects in two distinct object spaces, new space, and old space, each composed of sub-spaces. New space is used to create most new objects [^3]. New space is typically much smaller than old space and is collected much more frequently. New space is collected via a generation scavenger. Old space is collected with a mark-sweep collector.

### Scavenging New Space

New space is composed of the eden space, where objects are created, and two survivor spaces, past space, and future space. Eden is five times bigger than each survivor space, taking up five sevenths of new space. A table in old space, the "remembered table", holds thse objects in old space which refer to objects in new space. On scavenging future space is empty, eden contaims newly created objects, and past space contains objects that have survived a previous scavenge. The scavenger copies those objects in eden and past space reachable from the roots into future space, moving any objects that won't fit if future space fills up to old space, a process called "tenuring". The order of the copy arranges that objects in past space will be tenured before objects in eden, and that older objects in past space will be tenured before younger ones. After scavenging eden and past space are empty, and past and future spaces are swapped; past space becomes the now empty future space, and the (usually) non-empty future space now becomes past space [^4].

[^3]: large objects are created in old space, and pinned objects are migrated to old space when pinned. If facilities exist, objects can be instantiated as pinned objects, in which case they will be instantiated in old space.

[^4]: the rationale for this scheme is that in typical Smalltalk programs most objects die young, and the scavenger only spends time on objects that survive, so it collects the surviving objects, while wasting no effort reclaiming the space occupied by those objects that die. Long-lived objects are moved to old space where they are collectable but at higher cost, but affordably since old space is collected much less frequently than new space.

The roots of a scavenge are the live stack frames in the stack zone and the objects in the remembered table. Effectively the scavenger copies the transitive closure of new objects reachable from the stack zone and the remembered table to future survivor space, tenuring any that won't fit, with a bias towards tenuring older objects.

Ephemerons and weak arrays in the remembered table, or encountered during the scavenge, are "put to one side" and the objects they reference are not scavenged immediately. Once the first pass of scavenging has completed then any ephemerons whose keys have yet to be scavenged (are still in eden or past space) are triggered, and the references to new space objects from the ephemerons are scavenged. Note that this may encounter more ephemerons which may take another pass to resolve. These passes continue until no more triggerable ephemerons are found. Typically there is only ever a single pass because ephemerons referring to other ephemerons, while possible, are not typically created. Once all ephemeron passes are complete the weak arrays are processed and the slots referring to objects in eden or past space are nilled.

"Putting ephemerons and weak arrays to one side" is done in two ways. Ephemerons and weak arrays in the remembered table are moved to the end of the table for later scanning. Ephemerons and weak arrays that get scavenged (either to future space or tenured to old space) leave behind a corpse in eden or past space. The corpses are linked together in a singly linked list. Each corpse refers to its survivor so traversing the ephemeron list and the weak list visits the ephemerons and weak arrays that have survived the scavenge, be they in future space or old space.

### Mark-Sweeping Old Space

Old space is composed of a sequence of segments [^5]. On start up the system loads all objects in the snapshot into the first old space segment and allocates an empty segment for large allocations, tenured objects, and so on. Over time tenuring and large object allocation will cause old space to grow. New segments are added as needed or as directed by the programmer; their default size is controlled by ```Smalltalk vmParameterAt: 25```. Once old space has grown by some fraction, controlled by ```Smalltalk vmParameterAt: 55```, old space is collected, after first scavenging.

[^5]: Segments are joined together into a single linear space by ending each segment with a "bridge", which is a two word chunk that lies about its size, being large enough to reach the start of the next segment in the address space. Eden, past space and old space can be linked together using bridges and "slim bridges", one word objects, so that all live object spaces can be traversed in a single pass as if all objects were in a single segment.

The old space collector is a traditional mark sweep collector where a mark stack holds those objects reached that have yet to be scanned. Collection does a mark phase followed by the sweep phase. Starting from the live stack pages and the specialObjectsArray (in Smalltalk) objects are marked and added to the mark stack. While the mark stack is not empty the top object is taken off the mark stack, scanned, and any unmarked objects there-in are marked and pushed on the mark stack. Again weak objects and ephemerons are "put to one side". The strong slots of weak arrays are marked, but not the indexed slots, and the weak array is added to the "weaklingStack". An ephemeron that is marked has its key examined. If the key is marked it is reachable from the roots and the ephemeron is pushed on the mark stack like any ordinary object. Otherwise it is added to the unscannedEphemerons set. The unscannedEphemerons is simply a chunk of unused space, either eden (which is empty because a scavenge has been done), or the largest chunk of free space in old space, whichever is the larger. Once the mark stack is empty the unscannedEphemerons set is scanned and any ephemerons there-in that have unmarked keys are triggered. As the scan progresses the objects referred to by the ephemerons are marked, which may add newly encountered ephemerons to the unscannedEphemerons set, and the ephemeron removed from the set. This extended marking phase continues until the unscannedEphemerons set is empty. The final part of the mark phase then scans the weaklingStack and nils the slots that refer to unmarked objects, or objects in eden or future (which was past) space.

In the unlikely event that there are so many ephemerons that the unscannedEphemerons set fills up, remaining ephemerons are simply treated like normal objects. Hence it could be that triggerable ephemerons are not triggered, but they will be triggerable in a subsequent garbage collection. However, in practice this will never happen. Eden is at least big enough for half a million words, and it is extremely unlikely that any normal program would create half a million ephemerons.

To collect garbage the collector then sweeps all objects in all old space segments, adding any unmarked objects to free space [^6]. 

Finally, to reduce fragmentation, old space is compacted with a three finger algorithm that leaves pinned objects where they are and moves unpinned objects around them, eliminating as much free space as possible. The result is some combination of a large unused chunk at the end of the last used segment followed by zero or more empty segments, or free chunks around pinned objects in otherwise empty segments, etc. Spur does attempt to congregate pinned objects in a preferred segment to reduce fragmentation due to pinning.

[^6]: Spur manages free space in old space with a 64-element array. Free chunks of size one word through 63 words are kept in separate linked lists whose head is the array element of that size. Free chunks of 64 words or larger are kept in a semi-balanced tree where each node in the tree is the head of a list of chunks of the same size.

Eliot Miranda, Jan 2025

[comment]: <> (This document needs to reference Ungar's original Generation Scavenging paper, Ungar & Jackson's)
[comment]: <> (adaptive tenuring paper, and the VMMaker source for the VM, in particular SpurMemoryManager's)
[comment]: <> (class comment)

#### Footnotes
