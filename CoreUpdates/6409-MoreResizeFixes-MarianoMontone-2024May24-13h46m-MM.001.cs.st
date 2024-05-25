'From Cuis7.1 [latest update: #6393] on 24 May 2024 at 1:48:01 pm'!

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MM 5/24/2024 13:46:46'!
resizeBottom
	self resize: (self allowedArea top: self allowedArea center y)! !

!SystemWindow methodsFor: 'resize/collapse' stamp: 'MM 5/24/2024 13:46:28'!
resizeTop
	self resize: (self allowedArea bottom: self allowedArea center y)! !

