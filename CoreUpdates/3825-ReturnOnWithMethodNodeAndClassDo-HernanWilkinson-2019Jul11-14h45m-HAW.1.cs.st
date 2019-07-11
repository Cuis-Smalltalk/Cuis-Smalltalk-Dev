'From Cuis 5.0 [latest update: #3823] on 11 July 2019 at 5:10:57 pm'!

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'HAW 7/11/2019 16:14:04'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	"I have to do this because some codeProviders do no answer selectedClassOrMetaClass like the Workspace - Hernan"
	selectedClass := [ self codeProvider selectedClassOrMetaClass ] on: Error do: [ :anError | anError return: UndefinedObject ].
	[ methodNode := selectedClass methodNodeFor: model actualContents ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

