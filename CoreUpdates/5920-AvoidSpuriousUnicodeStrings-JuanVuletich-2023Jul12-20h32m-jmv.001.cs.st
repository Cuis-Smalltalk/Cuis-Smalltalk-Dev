'From Cuis 6.0 [latest update: #5917] on 12 July 2023 at 8:32:45 pm'!

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:27:23'!
initializeBottom
	selector _ #windowBottom:.
	cursorKey _ #resizeBottomCursor.
	self setBalloonText: `

'Bottom resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s bottom edge.
	
', 'Resize to bottom screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom half.

', 'Move to bottom screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom edge.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:23:50'!
initializeBottomLeft
	selector _ #windowBottomLeft:.
	cursorKey _ #resizeBottomLeftCursor.
	self setBalloonText: `

'Bottom Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom left corner.
	
', 'Resize to bottom left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom left quadrant.

', 'Move to bottom left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom left corner.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:24:46'!
initializeBottomRight
	selector _ #windowBottomRight:.
	cursorKey _ #resizeBottomRightCursor.
	self setBalloonText: `

'Bottom Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to bottom right corner.
	
', 'Resize to bottom right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s bottom right quadrant.

', 'Move to bottom right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s bottom right corner.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:17:25'!
initializeLeft
	selector _ #windowLeft:.
	cursorKey _ #resizeLeftCursor.
	self setBalloonText: `

'Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s left edge.
	
', 'Resize to left screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s left half.

', 'Move to left screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s left edge.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:26:52'!
initializeRight
	selector _ #windowRight:.
	cursorKey _ #resizeRightCursor.
	self setBalloonText: `

'Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to right edge.
	
', 'Resize to right screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s right half.

', 'Move to right screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s right edge.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:17:03'!
initializeTop
	selector _ #windowTop:.
	cursorKey _ #resizeTopCursor.
	self setBalloonText: `

'Top resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s top edge.
	
', 'Resize to top screen half' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screens''s top half.

', 'Move to top screen edge' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top edge.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:22:04'!
initializeTopLeft
	selector _ #windowTopLeft:.
	cursorKey _ #resizeTopLeftCursor.
	self setBalloonText: `

'Top Left resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top left corner.
	
', 'Resize to top left screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top left quadrant.

', 'Move to top left screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top left corner.'`! !

!WindowEdgeAdjustingMorph methodsFor: 'initialization' stamp: 'KLG 9/20/2022 14:22:17'!
initializeTopRight
	selector _ #windowTopRight:.
	cursorKey _ #resizeTopRightCursor.
	self setBalloonText: `

'Top Right resizing' bold, '
Press ', 'mouse button ' italic, '1' italic bold, ' and ', 'move the mouse' italic, ' to move the window''s to top right corner.
	
', 'Resize to top right screen quadrant' bold, '
Press ', 'mouse button ' italic, '2' italic bold, ' to resize the window to the screen''s top right quadrant.

', 'Move to top right screen corner' bold, '
Press the ', 'shift ' italic bold, ' key' italic, ' and ', 'mouse button ' italic, '2' italic bold, ' to move the window to the screen''s top right corner.'`! !

