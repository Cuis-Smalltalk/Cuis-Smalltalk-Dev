'From Cuis 4.2 of 25 July 2013 [latest update: #2125] on 12 December 2014 at 9:24:23.4983 am'!

!Stream methodsFor: 'testing' stamp: 'jmv 12/12/2014 09:14'!
closed
	"Answer true if we have been closed and are no longer usable.
	Meaningful, for example, for file streams."
	^ false! !

!methodRemoval: TestResult #initialize!
TestResult removeSelector: #initialize!
!methodRemoval: ReadWriteStream #close!
ReadWriteStream removeSelector: #close!
!methodRemoval: ReadWriteStream #closed!
ReadWriteStream removeSelector: #closed!
!methodRemoval: WriteStream #flush!
WriteStream removeSelector: #flush!
!methodRemoval: ReadStream #openReadOnly!
ReadStream removeSelector: #openReadOnly!
!methodRemoval: ReadStream #readOnly!
ReadStream removeSelector: #readOnly!
!methodRemoval: DummyStream #close!
DummyStream removeSelector: #close!
!methodRemoval: RemoteString class #initialize!
RemoteString class removeSelector: #initialize!
RemoteString initialize!
!methodRemoval: PopUpMenu class #initialize!
PopUpMenu class removeSelector: #initialize!
PopUpMenu initialize!
!methodRemoval: TempVariableNode #printOn:indent:!
TempVariableNode removeSelector: #printOn:indent:!
!methodRemoval: MouseEvent #eventPosition!
MouseEvent removeSelector: #eventPosition!
!methodRemoval: InnerTextMorph class #includeInNewMorphMenu!
InnerTextMorph class removeSelector: #includeInNewMorphMenu!
!methodRemoval: InnerTextMorph #isOpaqueMorph!
InnerTextMorph removeSelector: #isOpaqueMorph!
!methodRemoval: InnerListMorph class #includeInNewMorphMenu!
InnerListMorph class removeSelector: #includeInNewMorphMenu!
!methodRemoval: MessageSetWindow #update:!
MessageSetWindow removeSelector: #update:!
!methodRemoval: Morph class #initialize!
Morph class removeSelector: #initialize!
Morph initialize!
!methodRemoval: Integer #printOn:base:!
Integer removeSelector: #printOn:base:!
!methodRemoval: Association #isVariableBinding!
Association removeSelector: #isVariableBinding!
!methodRemoval: ContextPart #pc!
ContextPart removeSelector: #pc!
!methodRemoval: DisplayScreen #copyBits:from:at:clippingBox:rule:fillColor:!
DisplayScreen removeSelector: #copyBits:from:at:clippingBox:rule:fillColor:!
!methodRemoval: DisplayScreen #copyBits:from:at:clippingBox:rule:fillColor:map:!
DisplayScreen removeSelector: #copyBits:from:at:clippingBox:rule:fillColor:map:!
!methodRemoval: ColorForm #displayOn:at:clippingBox:rule:fillColor:!
ColorForm removeSelector: #displayOn:at:clippingBox:rule:fillColor:!
!methodRemoval: Compiler class #initialize!
Compiler class removeSelector: #initialize!
Compiler initialize!
!methodRemoval: SkipList #remove:!
SkipList removeSelector: #remove:!
!methodRemoval: SourceFileArray #collect:!
SourceFileArray removeSelector: #collect:!
!methodRemoval: Debugger #selectedClassOrMetaClass!
Debugger removeSelector: #selectedClassOrMetaClass!
!methodRemoval: Debugger #selectedMessageCategoryName!
Debugger removeSelector: #selectedMessageCategoryName!
!methodRemoval: StrikeFont #bold!
StrikeFont removeSelector: #bold!
!methodRemoval: StrikeFont #boldItalic!
StrikeFont removeSelector: #boldItalic!
!methodRemoval: StrikeFont #italic!
StrikeFont removeSelector: #italic!
!methodRemoval: StrikeFont #struckThrough!
StrikeFont removeSelector: #struckThrough!
!methodRemoval: StrikeFont #underlined!
StrikeFont removeSelector: #underlined!
