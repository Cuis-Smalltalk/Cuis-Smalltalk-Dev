'From Cuis 5.0 [latest update: #4541] on 22 March 2021 at 5:18:56 pm'!

!Preferences class methodsFor: 'personalization' stamp: 'jmv 3/22/2021 17:18:15'!
stylingWithEmphasisInWorkspaces
	^ self
		valueOfFlag: #stylingWithEmphasisInWorkspaces
		ifAbsent: [false]! !


!Workspace methodsFor: 'user interface support' stamp: 'jmv 3/22/2021 17:17:29'!
allowStylingWithEmphasis
	"Disabled by default for faster styling of large contents, as text metrics are not affected by styling."

	^ Preferences stylingWithEmphasisInWorkspaces! !

