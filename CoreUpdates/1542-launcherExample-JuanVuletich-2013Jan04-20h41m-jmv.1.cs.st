'From Cuis 4.1 of 12 December 2012 [latest update: #1539] on 4 January 2013 at 8:43:24 pm'!

!LayoutMorph class methodsFor: 'examples' stamp: 'jmv 1/4/2013 13:42'!
launcherExample
	"
	self launcherExample
	"
	| b1 b2 b3 row b4 random buttons |
	random _ Random new.
	b1 _ PluggableButtonMorph model: [ Date today print ] action: #value label: 'Date'.
	b2 _ PluggableButtonMorph model: [ Time now print ] action: #value label: 'Time'.
	b3 _ PluggableButtonMorph model: [ SystemVersion current print ] action: #value label: 'Version'.
	b4 _ PluggableButtonMorph model: [ random next print ] action: #value label: 'Random'.
	buttons _ {b1. b2. b3. b4}.
	buttons do: [ :button |
		button color: Color lightRed ].
	row _ LayoutMorph newRow
		 color: Color red;
		 addMorphs: buttons;
		 morphExtent: 300 @ 40.
	^ row openInWorld! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 1/4/2013 13:31'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 _ Switch new.
	s2 _ Switch new turnOn.
	s3 _ Switch new.
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 _ (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 _ (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 _ (PluggableButtonMorph model: s3 stateGetter: #isOn action: #turnOn) label: 'S3'.
	b1 color: Color lightRed.
	b2 color: Color lightRed.
	b3 color: Color lightRed.
	row _ LayoutMorph newRow
		addMorphs: (Array with: b1 with: b2 with: b3);
		morphExtent: 120@35.
	^ row
! !

