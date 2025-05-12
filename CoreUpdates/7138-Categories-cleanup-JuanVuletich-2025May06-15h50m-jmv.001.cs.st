'From Cuis7.3 [latest update: #7127] on 6 May 2025 at 3:55:08 pm'!

!UnicodeSymbol class reorganize!
('instance creation' newFrom: readFrom:)
('private' basicFromString:)
!


!String class reorganize!
('minimalKernel - Bootstrap' caseSensitiveOrder compare:with:collated: is:equalTo: isAscii: new: with:)
('minimalKernel - Symbol support' symbolClassToUse)
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder lowercasingTable tokenishTable uppercasingTable)
!


!InstructionStream class reorganize!
('instance creation' on:)
('compiling' instVarNamesAndOffsetsDo: isContextClass)
('class initialization' initialize)
!


!BoxedFloat64 reorganize!
('minimalKernel - Float support' * + - / < <= = > >= ~= exponent ln timesTwoPower: truncated)
('mathematical functions' arcTan cos exp primSqrt raisedTo: raisedToInteger: sin)
('truncation and round off' fractionPart)
('testing' isDenormalized isFinite isInfinite isNaN sign)
('printing' storeOn:base:)
!


!SmallFloat64 reorganize!
('minimalKernel - Float support' * + - / < <= = > >= ~= exponent identityHash ln timesTwoPower: truncated)
('mathematical functions' arcTan cos exp primSqrt profilerFriendlyPrimTimesTwoPower: profilerFriendlyTimesTwoPower: sin)
('truncation and round off' fractionPart)
('copying' shallowCopy)
!

