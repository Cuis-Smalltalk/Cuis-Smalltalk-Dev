Class {
	#name : 'SerializableClosureDecompilerConstructor',
	#superclass : 'DecompilerConstructor',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'constructor' }
SerializableClosureDecompilerConstructor >> codeAssignTo: variable value: expression [

	^ SerializableClosureAssignmentNode new variable: variable value: expression
]

{ #category : 'constructor' }
SerializableClosureDecompilerConstructor >> codeInst: index [

	^ SerializableClosureInstanceVariableNode new
		name: (instVars at: index + 1 ifAbsent: ['unknown', index printString])
		index: index + 1
]

{ #category : 'constructor' }
SerializableClosureDecompilerConstructor >> codeTemp: index [

	^ SerializableClosureTempVariableNode new
		name: 'temp' , (index + 1) printString
		index: index
		type: LdTempType
		scope: 0
]

{ #category : 'constructor' }
SerializableClosureDecompilerConstructor >> codeTemp: index named: tempName [

	^ SerializableClosureTempVariableNode new
		name: tempName
		index: index
		type: LdTempType
		scope: 0
]
