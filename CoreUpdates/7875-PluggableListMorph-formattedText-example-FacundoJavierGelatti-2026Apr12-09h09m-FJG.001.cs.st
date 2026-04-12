'From Cuis7.7 [latest update: #7874] on 12 April 2026 at 9:12:09 am'!

!PluggableListMorph class methodsFor: 'examples' stamp: 'FJG 12/Apr/2026 09:11:34'!
example01FormattedText
	"
	PluggableListMorph example01FormattedText
	"
	PluggableListMorph
		withModel: {
			(Text string: 'Bold' attributes: {TextEmphasis bold}) , (Text string: ' and red' attributes: {TextColor color: Color red}).
			(Text string: 'Blue ' attributes: {TextColor color: Color blue}), (Text string: 'italic' attributes: {TextEmphasis italic}).
			(Text string: 'Normal ' attributes: {TextEmphasis normal}) , (Text string: 'strikethrough' attributes: {TextEmphasis struckThrough})
		}
		listGetter: #yourself
		indexGetter: nil
		indexSetter: nil
		:: openInHand :: morphExtent: 400@100.! !


!PluggableListMorph class reorganize!
('instance creation' withModel:listGetter:indexGetter:indexSetter:)
('examples' example01FormattedText)
!

