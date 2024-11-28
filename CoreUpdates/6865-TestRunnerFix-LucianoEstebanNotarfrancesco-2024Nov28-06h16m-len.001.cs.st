'From Cuis7.1 [latest update: #6864] on 28 November 2024 at 6:19:50 am'!

!TestRunner methodsFor: 'updating' stamp: 'len 10/9/2024 09:27:38'!
update: aParameter 
	"updates come in from another thread"
	(aParameter is: #TestResult) ifTrue:
		[UISupervisor whenUIinSafeState:
			[result := aParameter.
"			self updatePassFail: aParameter."
			self updateDetails: aParameter.
			self updateFailures: aParameter.
			self updateErrors: aParameter].
		^ self].
	(aParameter is: #TestCase) ifTrue:
		[UISupervisor whenUIinSafeState:
			[completedTests := completedTests + 1.
			progressLabel := aParameter printString.
			self changed: #progress].
		^ self].
	super update: aParameter! !


!ProgressMorph methodsFor: 'accessing' stamp: 'len 10/11/2024 08:39:02'!
label: aString subLabel: otherString
	| label subLabel n |
	label := aString.
	subLabel := otherString.
	layoutSpec notNil ifTrue: [
		label isEmpty ifFalse: [
			n := label size * extent x * 0.95 // (FontFamily defaultFamilyAndPointSize widthOfString: label).
			label := label squeezedTo: n ].
		subLabel isEmpty ifFalse: [
			n := subLabel size * extent x * 0.95  // (FontFamily defaultFamilyAndPointSize widthOfString: subLabel).
			subLabel := subLabel squeezedTo: n ] ].
	self whenUIinSafeState: [
		labelMorph contents: label.
		subLabelMorph contents: subLabel.
		self updatePositionAndExtent. ]! !


