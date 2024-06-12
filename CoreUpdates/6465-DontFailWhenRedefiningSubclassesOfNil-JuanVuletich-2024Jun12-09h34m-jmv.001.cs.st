'From Cuis7.1 [latest update: #6464] on 12 June 2024 at 9:40:54 am'!

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 6/12/2024 09:40:21'!
update: oldClass to: newClass
	"Convert oldClass, all its instances and possibly its meta class into newClass,
	 instances of newClass and possibly its meta class. The process is surprisingly
	 simple in its implementation and surprisingly complex in its nuances and potentially
	 bad side effects.
	 We can rely on two assumptions (which are critical):
		#1: The method #updateInstancesFrom: will not create any lasting pointers to
			 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do
			 a become of the old vs. the new instances and therefore it will not create
			 pointers to *new* instances before the #become: which are *old* afterwards)
		#2: The non-preemptive execution of the critical piece of code guarantees that
			 nobody can get a hold by 'other means' (such as process interruption and
			 reflection) on the old instances.
	 Given the above two, we know that after #updateInstancesFrom: there are no pointers
	 to any old instances. After the forwarding become there will be no pointers to the old
	 class or meta class either.
	 Andreas Raab, 2/27/2003 23:42"
	| meta allInstances |
	meta := oldClass isMeta.
	"Note: Everything from here on will run without the ability to get interrupted
	to prevent any other process to create new instances of the old class."
	["Note: The following removal may look somewhat obscure and needs an explanation.
	  When we mutate the class hierarchy we create new classes for any existing subclass.
	  So it may look as if we don't have to remove the old class from its superclass. However,
	  at the top of the hierarchy (the first class we reshape) that superclass itself is not newly
	  created so therefore it will hold both the oldClass and newClass in its (obsolete or not)
	  subclasses. Since the #become: below will transparently replace the pointers to oldClass
	  with newClass the superclass would have newClass in its subclasses TWICE. With rather
	  unclear effects if we consider that we may convert the meta-class hierarchy itself (which
	  is derived from the non-meta class hierarchy).
	  Due to this problem ALL classes are removed from their superclass just prior to converting
	  them. Here, breaking the superclass/subclass invariant really doesn't matter since we will
	  effectively remove the oldClass (becomeForward:) just a few lines below."

		"Convert the instances of oldClass into instances of newClass.
		Note: #canUpdateFrom: has already been called. Instance mutation is safe wrt methods currently in execution."
		allInstances := oldClass allInstances.
		allInstances notEmpty ifTrue: [
			newClass updateInstances: allInstances asArray from: oldClass isMeta: meta ].

		oldClass superclass ifNotNil: [ :oldSuperclass |
			oldSuperclass removeSubclass: oldClass.
			oldSuperclass removeObsoleteSubclass: oldClass ].

		"make sure that the VM cache is clean"
		oldClass methodDict do: [:cm | cm flushCache].

		meta
			ifTrue: [
				oldClass becomeForward: newClass.
				oldClass updateMethodBindingsTo: oldClass binding]
			ifFalse: [
				{oldClass. oldClass class} elementsForwardIdentityTo: {newClass. newClass class}.
				oldClass updateMethodBindingsTo: oldClass binding.
				oldClass class updateMethodBindingsTo: oldClass class binding].

		"eem 5/31/2014 07:22 At this point there used to be a garbage collect whose purpose was
		 to ensure no old instances existed after the becomeForward:.  Without the GC it was possible
		 to resurrect old instances using e.g. allInstancesDo:.  This was because the becomeForward:
		 updated references from the old objects to new objects but didn't destroy the old objects.
		 But as of late 2013/early 2014 becomeForward: has been modified to free all the old objects."
	] valueUnpreemptively! !

