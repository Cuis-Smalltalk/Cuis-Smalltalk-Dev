'From Cuis7.9 [latest update: #8026] on 26 June 2026 at 10:14:57 am'!
!methodRemoval: NewInstanceVariablePrecondition #valueForSuperclass stamp: 'HAW 26/Jun/2026, 1:10:50 pm (UTC)'!
NewInstanceVariablePrecondition removeSelector: #valueForSuperclass!
!methodRemoval: ParametersSizeChange #newImplementors stamp: 'HAW 26/Jun/2026, 1:11:25 pm (UTC)'!
ParametersSizeChange removeSelector: #newImplementors!
!methodRemoval: ParametersSizeChange #changedSenders stamp: 'HAW 26/Jun/2026, 1:11:28 pm (UTC)'!
ParametersSizeChange removeSelector: #changedSenders!
!methodRemoval: RenameObject #newName stamp: 'HAW 26/Jun/2026, 1:09:39 pm (UTC)'!
RenameObject removeSelector: #newName!
!methodRemoval: InlineTemporaryVariable #closestAssignmentBeforeUsage: stamp: 'HAW 26/Jun/2026, 1:09:12 pm (UTC)'!
InlineTemporaryVariable removeSelector: #closestAssignmentBeforeUsage:!
!methodRemoval: InlineTemporaryVariable #rangeIsPartOfTemporaryDeclaration: stamp: 'HAW 26/Jun/2026, 1:09:22 pm (UTC)'!
InlineTemporaryVariable removeSelector: #rangeIsPartOfTemporaryDeclaration:!
!methodRemoval: InlineTemporaryVariable #tempNodeToInline stamp: 'HAW 26/Jun/2026, 1:09:27 pm (UTC)'!
InlineTemporaryVariable removeSelector: #tempNodeToInline!
!methodRemoval: AddInstanceVariable #classToRefactor stamp: 'HAW 26/Jun/2026, 1:09:01 pm (UTC)'!
AddInstanceVariable removeSelector: #classToRefactor!
!methodRemoval: MessageNodeReference #isPossibleMessageSend stamp: 'HAW 26/Jun/2026, 1:12:43 pm (UTC)'!
MessageNodeReference removeSelector: #isPossibleMessageSend!
!methodRemoval: MessageNodeReference #asPossibleMessageNodeReference stamp: 'HAW 26/Jun/2026, 1:13:06 pm (UTC)'!
MessageNodeReference removeSelector: #asPossibleMessageNodeReference!

!ParametersSizeChange reorganize!
('rename senders - private' renameSendersIn:)
('initialization' initializeFrom:to:implementors:senders:)
('create new implementors - private' createNewImplementorOf:)
!

!classRemoval: #PossibleMessageNodeReference stamp: 'HAW 26/Jun/2026, 1:13:31 pm (UTC)'!
Smalltalk removeClassNamed: #PossibleMessageNodeReference!

