'From Cuis 5.0 [latest update: #4139] on 8 May 2020 at 12:00:19 pm'!

!Morph methodsFor: 'caching' stamp: 'jmv 5/8/2020 11:43:26'!
clearId
	id _ nil.! !


!Morph class methodsFor: 'initialize-release' stamp: 'jmv 5/8/2020 11:53:06'!
releaseClassState

	LastMorphId _ nil.
	self allSubInstancesDo: [ :each |
		each clearId.
		each releaseCachedState ]! !


!Behavior methodsFor: 'system startup' stamp: 'jmv 5/8/2020 11:39:03'!
releaseClassCachedState
	"Will be called for each class on shutdown or snapshot.
	All class vars or class instVar vars that can be cheaply recreated lazily on demand, should be nilled.
	For more expensive stuff to clean and recreate, consider #releaseClassState that is not called on every image save.
	See implementors for examples"! !

!Behavior methodsFor: 'system startup' stamp: 'jmv 5/8/2020 11:40:43'!
releaseClassState
	"Will be called for each class on 'Save as new version'.
	All class vars or class instVar vars that can be recreated lazily on demand, should be nilled. This is done not only to save space, but more importantly, to prepare Cuis for a complete bootstrap from sources. For this, it should be possible to recreate all class state, at least with default values.
	See implementors for examples"! !


!Morph methodsFor: 'caching' stamp: 'jmv 5/8/2020 11:44:22'!
releaseCachedState
	"Release any state that can be recomputed on demand, such as the pixel values for a color gradient or the editor state for a TextMorph. This method may be called to save space when a morph becomes inaccessible. Implementations of this method should do 'super releaseCachedState'."! !

!methodRemoval: Morph class #releaseClassCachedState stamp: 'jmv 5/8/2020 11:59:19'!
Morph class removeSelector: #releaseClassCachedState!
