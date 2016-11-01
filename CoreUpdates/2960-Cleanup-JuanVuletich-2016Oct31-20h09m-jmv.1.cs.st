'From Cuis 4.2 of 25 July 2013 [latest update: #2950] on 31 October 2016 at 8:17:51 pm'!
!methodRemoval: SelectorNode #emitForValue:on:!
SelectorNode removeSelector: #emitForValue:on:!
!methodRemoval: SelectorNode #sizeForValue:!
SelectorNode removeSelector: #sizeForValue:!
!methodRemoval: Collection #toBraceStack:!
Collection removeSelector: #toBraceStack:!

!SelectorNode reorganize!
('visiting' accept:)
('code generation' reserve:)
('code generation (new scheme)' emitCode:args:encoder: emitCode:args:encoder:super: emitCodeForEffect:encoder: emitCodeForValue:encoder: sizeCode:args:super: sizeCodeForEffect: sizeCodeForValue:)
('testing' isForFFICall isPvtSelector)
('printing' key: printAsFFICallWithArguments:on:indent: printOn:indent: printWithClosureAnalysisOn:indent:)
('initialization' name:key:index:type:)
!

