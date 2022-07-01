'From Cuis 6.0 [latest update: #5341] on 30 June 2022 at 7:53:07 pm'!

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 6/30/2022 19:51:02'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray selector |
	selector _ machinePref caseOf: {
			[#default]		 		-> [#machineDefault].
			[#slow] 				-> [#machineSlow].
			[#smalltalk80] 				-> [#machineSmalltalk80] }
		otherwise: [ #default ].
	prefArray _ self perform: selector.
	prefArray do: [:array | 		self sysPreferences at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil]! !

!methodRemoval: PreferenceSet class #migrate stamp: 'jmv 6/30/2022 19:35:56'!
PreferenceSet class removeSelector: #migrate!
