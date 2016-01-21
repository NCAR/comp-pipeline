; docformat = 'rst'

;+
; Function which computes the Mueller matrix of a linear polarizer
; given its transmission and orientation angle. Inputs:
;	trans: The transmission.
;	angle: The angle, in degrees.
;
; Returns the 4x4 mueller matrix.
;
; Joseph Plowman
;-
function comp_mueller_polarizer,trans,angle

	ang=angle*!dpi/180.d0  ;convert to radians
	c2=cos(2.d0*ang) & s2=sin(2.d0*ang)

	matrix=trans*[[1.d0,c2,s2,0.d0],$
		[c2,c2^2,c2*s2,0.d0],$
		[s2,s2*c2,s2^2,0.d0],$
		[0.d0,0.d0,0.d0,0.d0]]

	return,matrix
end


