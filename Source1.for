      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     2 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,
     3 LACCFLA)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*8  FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),
     1 T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),
     1 COORD(*)
C
C Retrieve plastic strain components (PE)
      IF (CMNAME(1:5) .EQ. 'Shell' .AND. CMNAME(1:6) .EQ. 'Filter' .AND.
     1    CMNAME(1:10) .EQ. 'Layer') THEN
          CALL GETVRM('PE',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1    MATLAYO,LACCFLA)
          IF(JRCD.NE.0)THEN
          WRITE(6,*) 'REQUEST ERROR IN USDFLD FOR ELEMENT NUMBER ',
     1     NOEL,'INTEGRATION POINT NUMBER ',NPT
          ELSE
          FIELD(1) = ABS(ARRAY(4))  ! Plastic shear strain (PE12)
          STATEV(1) = FIELD(1)      ! Store in state variable
          ENDIF
C Set height-dependent field variable
      FIELD(2) = COORD(2)
      STATEV(2) = FIELD(2)
C      
      ELSEIF (CMNAME(1:4) .EQ. 'Core') THEN
          FIELD(1) = COORD(2)
          STATEV(1) = FIELD(1)
c          FIELD(3) = COORD(3)
c         STATEV(3) = FIELD(3)
      ENDIF
C
      RETURN
      END