'From Cuis 4.0 of 21 April 2012 [latest update: #1369] on 17 August 2012 at 8:44 pm'!

!Preferences class methodsFor: 'initialization' stamp: 'jmv 8/17/2012 20:43'!
initialize
	"
	Preferences initialize
	"
	Preferences cuisDefaults! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 8/17/2012 20:38'!
allowUnderscoreAssignments
	"Also see #allowUnderscoreSelectors.
	When true, $_ (left arrow / underscore) can be used as assignment operator.

	This preference is related with #allowUnderscoreSelectors. Combinations: If both preferences are false, underscore is illegal. If both preferences are true, underscore assignment needs the be surrounded by spaces and a single underscore cannot be used as selector."
	
"Important:
	If you set this preference to true, and the following character: _ looks like an underscore, evaluate
		StrikeFont useLeftArrow

	If you set this preference to false, be sure that #syntaxHighlightingAsYouTypeLeftArrowAssignment is also set to false.
	In addition, you'll most likely feel useful to set #syntaxHighlightingAsYouTypeAnsiAssignment to true
	Besides, if the following character: _ looks like a left arrow, you might prefer evaluating
		StrikeFont useUnderscore
	"
	^ self
		valueOfFlag: #allowUnderscoreAssignments
		ifAbsent: [ true ].

"
On 3/12/2010 8:14 PM, Andreas Raab wrote:
> Folks -
>
> Attached my take on selectors with underscores. It basically separates
> the issue of using underscores in assignment from the issue of using
> underscores in selectors, puts this into two individual preferences, and
> allows per-class scoping while providing a system-wide default.
>
> The possible combinations of preferences are:
>
> * allowUnderscoreAssignments off, allowUnderscoreSelectors: off
> The use of underscores is forbidden, i.e., the Croquet model.
>
> * allowUnderscoreAssignments on, allowUnderscoreSelectors: off
> The classic Squeak usage; all of the following are assignments:
>
> a _ b => a := b
> b_ c => b := c
> d _e => d := e
> f_g => f := g.
>
> * allowUnderscoreAssignments off, allowUnderscoreSelectors: on
> The standard usage in other dialects
>
> a _ b => ((a) _) b
> b_ c => (b_) c
> d _e => (d) _e
> f_g => (f_g)
>
> * allowUnderscoreAssignments on, allowUnderscoreSelectors: on
> The hybrid usage requiring spaces around underscore for assignment:
>
> a _ b => a := b
> b_ c => (b_) c
> d _e => (d) _e
> f_g => (f_g)
>
> This gives us a range of options to decide how to deal with it. I would
> personally say that for the core image we should go with the first
> option (disable underscores altogether) and only enable whichever option
> we like for the release.
>
> What do people think about this approach? I think it provides the most
> options and gives us ample flexibility to decide what we'd like to use
> down the road.
>
> If there is no fundamental opposition I'll push it into the trunk in a
> couple of days.
>
> Cheers,
> - Andreas
"! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 8/17/2012 20:38'!
allowUnderscoreSelectors
	"Also see #allowUnderscoreAssignments
	When true, $_ (left arrow / underscore) can be used in selectors and variable names.
	
This preference is related with #allowUnderscoreAssignments. Combinations: If both preferences are false, underscore is illegal. If both preferences are true, underscore assignment needs the be surrounded by spaces and a single underscore cannot be used as selector.
	
	If you set this preference to true, most likely you'll want character this: _ (character 95) to look like an underscore and not like a left arrow.
	For this, evaluate
		StrikeFont useUnderscore
	"
	^ self
		valueOfFlag: #allowUnderscoreSelectors
		ifAbsent: [ true ].

	"At #allowUnderscoreAssignments see the bottom comment from a mail to squeak-dev by Andreas (ar)"! !

!Preferences class methodsFor: 'themes' stamp: 'jmv 8/17/2012 20:42'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(fastDragWindowForMorphic false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(simpleMenus false)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(allowUnderscoreAssignments false)
		(allowUnderscoreSelectors false)
		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	)! !


!SHParserST80 methodsFor: 'aux' stamp: 'jmv 8/17/2012 20:36'!
allowUnderscoreAssignments
	"Query class + preference.
	Please see comments in Scanner >> #allowUnderscoreAssignments"
	^(classOrMetaClass ifNotNil: [ classOrMetaClass allowUnderscoreAssignments ])
		ifNil: [ Preferences allowUnderscoreAssignments ]! !

!SHParserST80 methodsFor: 'aux' stamp: 'jmv 8/17/2012 20:36'!
allowUnderscoreSelectors
	"Query class + preference.
	Please see comments in Scanner >> #allowUnderscoreSelectors"
	^(classOrMetaClass ifNotNil: [ classOrMetaClass allowUnderscoreSelectors ])
		ifNil: [ Preferences allowUnderscoreSelectors ]! !


!Scanner methodsFor: 'private' stamp: 'jmv 8/17/2012 20:35'!
allowUnderscoreAssignments
	"See comment at Preferences"
	^Preferences allowUnderscoreAssignments! !

!Scanner methodsFor: 'private' stamp: 'jmv 8/17/2012 20:35'!
allowUnderscoreSelectors
	"See comment at Preferences"
	^Preferences allowUnderscoreSelectors! !


!Parser methodsFor: 'private' stamp: 'jmv 8/17/2012 20:36'!
allowUnderscoreAssignments
	"Query class + preference"
	encoder == self ifTrue: [ ^super allowUnderscoreAssignments ].
	encoder ifNil: [ ^super allowUnderscoreAssignments ].
	^encoder classEncoding allowUnderscoreAssignments
		ifNil: [super allowUnderscoreAssignments]! !

Preferences initialize!
