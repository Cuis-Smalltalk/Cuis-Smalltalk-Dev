'From Cuis7.9 [latest update: #8073] on 22 July 2026 at 10:24:57 am'!

!Dialog methodsFor: 'drawing' stamp: 'jmv 22/Jul/2026, 1:23:49 pm (UTC)'!
drawOn: aCanvas
	aCanvas
		roundRect: self localBounds
		color: color
		radius: Theme current roundedWindowRadius.! !


!Dialog reorganize!
('initialization' buildWidgets)
('defaults' defaultColor defaultExtent defaultPadding)
('layout' beColumn)
('drawing' drawOn:)
('geometry' extentChanged: minimumExtent)
!

