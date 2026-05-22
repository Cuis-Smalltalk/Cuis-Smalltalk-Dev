Class {
	#name : 'RectangleTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Graphics-Primitives',
	#package : 'BaseImageTests',
	#tag : 'Graphics-Primitives'
}

{ #category : 'testing' }
RectangleTest >> testHasPositiveExtent [
	self assert: (10@10 corner: 20@20) hasPositiveExtent.
	self deny: (10@10 corner: 2@2) hasPositiveExtent.
]

{ #category : 'testing' }
RectangleTest >> testIsRectangle [
	self assert: (Rectangle new is: #Rectangle)
]

{ #category : 'testing' }
RectangleTest >> testMerge [
	self assert: ((10@10 corner: 20@20) merge: (10@10 corner: 2@2)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) merge: (10@10 corner: 20@2)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) merge: (10@10 corner: 2@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 2@2) merge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@2) merge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 2@20) merge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) merge: (50@50 corner: 60@60)) equals: (10@10 corner: 60@60).
	self assert: ((10@10 corner: 20@20) merge: (1000@10 corner: 200@2)) equals: (10@10 corner: 20@20).
]

{ #category : 'testing' }
RectangleTest >> testQuickMerge [
	self assert: ((10@10 corner: 20@20) quickMerge: (10@10 corner: 2@2)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) quickMerge: (10@10 corner: 20@2)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) quickMerge: (10@10 corner: 2@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 2@2) quickMerge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@2) quickMerge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 2@20) quickMerge: (10@10 corner: 20@20)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) quickMerge: (50@50 corner: 60@60)) equals: (10@10 corner: 60@60).
	self assert: ((10@10 corner: 20@20) quickMerge: (1000@10 corner: 200@2)) equals: (10@10 corner: 20@20).
]

{ #category : 'testing' }
RectangleTest >> testQuickMergeIntersectionWith [
	self assert: ((10@10 corner: 20@20) quickMerge: (50@50 corner: 60@60) intersectionWith: (0@0 corner: 100@100)) equals: (10@10 corner: 60@60).
	self assert: ((10@10 corner: 20@20) quickMerge: (50@50 corner: 60@60) intersectionWith: (0@0 corner: 55@55)) equals: (10@10 corner: 55@55).
	self assert: ((10@10 corner: 20@20) quickMerge: (50@50 corner: 60@60) intersectionWith: (0@0 corner: 35@35)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) quickMerge: (50@50 corner: 60@60) intersectionWith: (30@30 corner: 35@35)) equals: (10@10 corner: 20@20).
	self assert: ((10@10 corner: 20@20) quickMerge: (500@50 corner: 60@60) intersectionWith: (30@30 corner: 35@35)) equals: (10@10 corner: 20@20).
]
