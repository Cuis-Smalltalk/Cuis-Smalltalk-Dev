'From Cuis 4.1 of 12 December 2012 [latest update: #1620] on 2 March 2013 at 6:43:43 pm'!

!Scanner methodsFor: 'expression types' stamp: 'raa 3/2/2013 11:55'!
scanForFindSelectorUpTo: terminator
"
Scanner findSelectorTests
"
    | s |

    s := WriteStream on: (String new: 100).
    [tokenType == terminator or: [tokenType == #doIt]] whileFalse: [
        tokenType caseOf: {
            [#leftParenthesis] -> [self scanToken; scanForFindSelectorUpTo: #rightParenthesis].
            [#leftBracket] -> [self scanToken; scanForFindSelectorUpTo: #rightBracket].
            [#leftBrace] -> [self scanToken; scanForFindSelectorUpTo: #rightBrace].
            [#keyword] -> [s nextPutAll: token].
        } otherwise: [].
        self scanToken
    ].
    ^s contents! !


!Scanner class methodsFor: 'testing' stamp: 'raa 3/2/2013 12:05'!
findSelectorIn: aString

    | result |
    result _ Scanner new
        scan: (ReadStream on:  aString);
        scanForFindSelectorUpTo: #notATerminator.
    ^result! !

!Scanner class methodsFor: 'testing' stamp: 'raa 3/2/2013 12:02'!
findSelectorTests
"
Scanner findSelectorTests explore
"
    ^#(
        '3 perform: #between:and: with: 1 with: 5'
        'self do: [:i | ] separatedBy: [] '
        'self perform: #add: with: anObject'
        'self do: [:i | i do: [ :each | ] ] separatedBy: [] '
        'self x: {self y:} y: ''self new: 100'''
        'a max:b'
        'a'
    ) collect: [ :e |
        {e. self findSelectorIn: e}
    ].! !


!String methodsFor: 'converting' stamp: 'raa 3/2/2013 12:02'!
findSelector
    "Revised to use scanner for better removal of extraneous stuff"

    | sel |

    sel := self withBlanksTrimmed.
    (sel includes: $:) ifTrue: [sel _ Scanner findSelectorIn: sel].
    sel isEmpty ifTrue: [^ nil].
    Symbol hasInterned: sel ifTrue:
        [:aSymbol | ^ aSymbol].
    ^ nil! !

