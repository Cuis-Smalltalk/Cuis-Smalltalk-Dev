'From Cuis 4.2 of 25 July 2013 [latest update: #1992] on 24 May 2014 at 11:43:42.027456 am'!
!methodRemoval: CompiledMethod class #abstractMarkers!
CompiledMethod class removeSelector: #abstractMarkers!
!methodRemoval: CompiledMethod class #conflictMarker!
CompiledMethod class removeSelector: #conflictMarker!
!methodRemoval: CompiledMethod class #disabledMarker!
CompiledMethod class removeSelector: #disabledMarker!
!methodRemoval: CompiledMethod class #explicitRequirementMarker!
CompiledMethod class removeSelector: #explicitRequirementMarker!
!methodRemoval: CompiledMethod class #implicitRequirementMarker!
CompiledMethod class removeSelector: #implicitRequirementMarker!
!methodRemoval: CompiledMethod class #subclassResponsibilityMarker!
CompiledMethod class removeSelector: #subclassResponsibilityMarker!

!CompiledMethod class reorganize!
('class initialization' fullFrameSize initialize smallFrameSize)
('instance creation' basicNew: new new: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: newBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:flag: newFrom: newInstanceFrom:variable:size:map: newMethod:header: primitive:numArgs:numTemps:stackSize:literals:bytecodes:trailer: toReturnConstant:trailerBytes: toReturnField:trailerBytes: toReturnSelf toReturnSelfTrailerBytes:)
('services' timeStamp:partsDo: unboundMethods)
('evaluating' receiver:withArguments:executeMethod:)
!

!methodRemoval: CompiledMethod #isAbstract!
CompiledMethod removeSelector: #isAbstract!
!methodRemoval: CompiledMethod #isConflict!
CompiledMethod removeSelector: #isConflict!
!methodRemoval: CompiledMethod #isDisabled!
CompiledMethod removeSelector: #isDisabled!
!methodRemoval: CompiledMethod #isDisabled:!
CompiledMethod removeSelector: #isDisabled:!
!methodRemoval: CompiledMethod #isExplicitlyRequired!
CompiledMethod removeSelector: #isExplicitlyRequired!
!methodRemoval: CompiledMethod #isExplicitlyRequired:!
CompiledMethod removeSelector: #isExplicitlyRequired:!
!methodRemoval: CompiledMethod #isImplicitlyRequired!
CompiledMethod removeSelector: #isImplicitlyRequired!
!methodRemoval: CompiledMethod #isImplicitlyRequired:!
CompiledMethod removeSelector: #isImplicitlyRequired:!
!methodRemoval: CompiledMethod #isProvided!
CompiledMethod removeSelector: #isProvided!
!methodRemoval: CompiledMethod #isProvided:!
CompiledMethod removeSelector: #isProvided:!
!methodRemoval: CompiledMethod #isRequired!
CompiledMethod removeSelector: #isRequired!
!methodRemoval: CompiledMethod #isRequired:!
CompiledMethod removeSelector: #isRequired:!
!methodRemoval: CompiledMethod #isSubclassResponsibility!
CompiledMethod removeSelector: #isSubclassResponsibility!
!methodRemoval: CompiledMethod #isSubclassResponsibility:!
CompiledMethod removeSelector: #isSubclassResponsibility:!
!methodRemoval: CompiledMethod #markerOrNil!
CompiledMethod removeSelector: #markerOrNil!
