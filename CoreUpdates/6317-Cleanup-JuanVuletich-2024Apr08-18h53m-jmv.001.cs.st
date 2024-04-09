'From Cuis6.3 [latest update: #6316] on 8 April 2024 at 6:54:14 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 4/8/2024 15:51:00'!
classList

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self defaultClassList].! !

!Browser methodsFor: 'initialization' stamp: 'jmv 4/8/2024 15:51:51'!
initialize
	super initialize.
	self systemOrganizer: SystemOrganization.
	listClassesHierarchically := Preferences at: #listClassesHierarchically! !

!methodRemoval: Browser class #listClassesHierarchically stamp: 'jmv 4/8/2024 18:53:25'!
Browser class removeSelector: #listClassesHierarchically!
!methodRemoval: Browser class #new stamp: 'jmv 4/8/2024 18:53:25'!
Browser class removeSelector: #new!
!methodRemoval: Browser #listClassesHierarchically stamp: 'jmv 4/8/2024 18:53:25'!
Browser removeSelector: #listClassesHierarchically!
!methodRemoval: Browser #initializeListClassesHierachically stamp: 'jmv 4/8/2024 18:53:25'!
Browser removeSelector: #initializeListClassesHierachically!

!Browser class reorganize!
('cached state access' recentClasses releaseClassCachedState)
('class list' disableListClassesHierarchically enableListClassesHierarchically)
('ui' open)
!

