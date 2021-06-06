'From Cuis 5.0 [latest update: #4627] on 4 June 2021 at 3:38:24 pm'!

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 6/4/2021 15:35:46'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	"I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan"
	selectedClass := [ self codeProvider selectedClassOrMetaClass ] on: Error do: [ :anError | anError return: UndefinedObject ].
	[ 
		[ methodNode := selectedClass methodNodeFor: model actualContents asString ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

