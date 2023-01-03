'From Cuis 6.0 [latest update: #5611] on 3 January 2023 at 2:44:48 pm'!

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 1/2/2023 17:18:01'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray selector |
	selector _ machinePref caseOf: {
			[#default]		 		-> [#machineDefault].
			[#slow] 				-> [#machineSlow].
			[#smalltalk80] 				-> [#machineSmalltalk80] }
		otherwise: [ #machineDefault ].
	prefArray _ self perform: selector.
	prefArray do: [:array | 		self sysPreferences at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil].! !

