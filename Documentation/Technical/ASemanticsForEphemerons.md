# A semantics for Ephemerons (& Weak Referrers).

A system containing ephemerons and weak objects has four kinds of references:
⁃ normal, a.k.a. strong, references through variables
⁃ references from ephemerons through their key inst vars (the first inst var of each ephemeron)
⁃ references from ephemerons through their other inst vars
⁃ weak references (which are the indexed variables of weak arrays, whose named inst vars - if any - are normal, strong references)

In defining the semantics of ephemerons we are interested in two different transitive closures.

To define the first transitive closure the four categories of reference can be reduced to two, all references other than weak references, and weak references. The first category are “strong” references and include both kinds of reference from ephemerons.

The transitive closure of reachable objects from the system roots (which includes the current computation) through strong references derives the live set of objects. In any partial garbage collection the system may reclaim a subset of objects lying outside this set. In a full garbage collection, the system must reclaim all objects lying outside this set. In any garbage collection, partial or full, the system must nil indexed variables of weak objects referring to objects lying outside the set of live objects.

The second transitive closure is that obtained by following all strong references, but not following any reference (of either kind) from ephemerons. The set difference of the first and second transitive closures are those objects reachable only via ephemerons. Within this set there is a subset of the objects referred to by the key of any ephemeron within the first set (the set of all live objects). This subset is the set of keys reachable only through ephemerons. We shall call it the triggered keys set.

In any partial garbage collection the system may trigger some subset of the ephemerons whose keys are in the triggered keys set. In a full garbage collection the system should trigger all the ephemerons whose keys are in the triggered keys set, but may not if the number of these ephemerons is very large. However, repeated garbage collections must trigger all ephemerons whose keys are in the triggered keys set.

Note: “trigger” means “to trigger finalization of the Ephemeron”

Eliot Miranda, Dec 2024