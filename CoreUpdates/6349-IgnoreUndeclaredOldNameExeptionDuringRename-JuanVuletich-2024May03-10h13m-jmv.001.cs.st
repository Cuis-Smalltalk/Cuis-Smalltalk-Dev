'From Cuis6.3 [latest update: #6345] on 3 May 2024 at 10:16:18 am'!

!RenameObject methodsFor: 'applying - private' stamp: 'jmv 5/3/2024 10:16:13'!
newSourceCodeOf: aCompiledMethod

	| ranges newSource |
	ranges := [ self rangesToReplaceOf: aCompiledMethod ]
		on: UndeclaredVariableWarning
		do: [ :ex |
			ex variableName = oldName
				ifTrue: [
					"Ignore the UndeclaredVariableWarning due to the fact that we have
					already renamed a class / global, so the old code will always have 
					references to now invalid names, exactly those we are updating."
					ex resume ]
				ifFalse: [ ex pass ]].
	newSource := aCompiledMethod sourceCode copyReplacing: ranges with: newName.
	^newSource! !

