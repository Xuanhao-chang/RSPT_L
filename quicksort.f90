		SUBROUTINE SORT2(N, ARR, BRR)
		INTEGER N, M, NSTACK
		REAL ARR(N), BRR(N)
		PARAMETER (M=7, NSTACK=50)
! Sorts an array arr(1:n) into ascending order using Quicksort,
! while making the corresponding rearrangement of the array brr(1:n).
		INTEGER I, IR, J, JSTACK, K, l, ISTACK(NSTACK)
		REAL A, B, TEMP
		JSTACK = 0
		L 		 = 1
		IR     = N
1 	IF (ir-l .lt. M) THEN  !Insertion sort when subarray small enough.
		DO j=l+1,ir
			a=arr(j)
			b=brr(j)
			DO i=j-1,l,-1
				if(arr(i) .le. a) goto 2
				arr(i+1)=arr(i)
				brr(i+1)=brr(i)
			ENDDO
			i=l-1
2		  arr(i+1)=a
			brr(i+1)=b
		ENDDO
		IF (jstack .eq. 0) return
		ir=istack(jstack)
		l=istack(jstack-1)
		jstack=jstack-2
    ELSE
		k=(l+ir)/2
		temp=arr(k)
		arr(k)=arr(l+1)
		arr(l+1)=temp
		temp=brr(k)
		brr(k)=brr(l+1)
		brr(l+1)=temp
		if(arr(l).gt.arr(ir))then
			temp=arr(l)
			arr(l)=arr(ir)
			arr(ir)=temp
			temp=brr(l)
			brr(l)=brr(ir)
			brr(ir)=temp
		endif
		if(arr(l+1) .gt. arr(ir)) then
			temp=arr(l+1)
			arr(l+1)=arr(ir)
			arr(ir)=temp
			temp=brr(l+1)
			brr(l+1)=brr(ir)
			brr(ir)=temp
		endif
		if(arr(l) .gt. arr(l+1)) then
			temp=arr(l)
			arr(l)=arr(l+1)
			arr(l+1)=temp
			temp=brr(l)
			brr(l)=brr(l+1)
			brr(l+1)=temp
        endif
			i=l+1
			j=ir
			a=arr(l+1)
			b=brr(l+1)
3       continue
			i=i+1
			if(arr(i).lt.a) goto 3
4		continue
			j=j-1
			if(arr(j).gt.a) goto 4
			if(j.lt.i) goto 5
			temp=arr(i)
			arr(i)=arr(j)
			arr(j)=temp
			temp=brr(i)
			brr(i)=brr(j)
			brr(j)=temp
			goto 3
5 			arr(l+1)=arr(j)
			arr(j)=a
			brr(l+1)=brr(j)
			brr(j)=b
			jstack=jstack+2
			!Push pointers to larger subarray on stack, process smaller subarray immediately.
			if(jstack.gt.NSTACK) pause ’NSTACK too small in sort2’
			if(ir-i+1.ge.j-l)then
				istack(jstack)=ir
				istack(jstack-1)=i
				ir=j-1
			else
				istack(jstack)=j-1
				istack(jstack-1)=l
				l=i
			endif
        endif
    	goto 1
    END
