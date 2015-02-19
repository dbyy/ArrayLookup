// MedTool.PRG
// utility to test some fucntion on Mediator
// compile : clipper Medtool [/b] /n /w
// link    : blinker Medtool [, MrDebug.lib] lib oslib [out name.exe]
// or use MKE.BAT
// 21-Mar-2011 Chowdary - Task #12178 Mediator upgrade V 4.3.3.1 - no changes necessary
// 30-Jun-2104 fmp test call of PL/SQL package through Mediator

// ********************************************************************************

#INCLUDE"MEDIATOR.CH"
#INCLUDE "BOX.CH"

/*
#INCLUDE"MISC.CH"
#INCLUDE"BF.CH"
#INCLUDE "SETCURS.CH"
#INCLUDE"FILEIO.CH"
#INCLUDE"SET.CH"
#INCLUDE"BOX.CH"
#INCLUDE"DBSTRUCT.CH"
#DEFINE  LOG_PATH        1        // path for claim log file
#DEFINE  LOG_FNAME       2        // (tmp) file name of claim log
#DEFINE  LOG_FHANDLE     3        // DOS file handle for claim log file
*/

* #INCLUDE"INKEY.CH"

* request MEDNTX

STATIC saLog
static cScreen

STATIC nRow

// ********************************************************************************
INIT PROCEDURE _entrypoint()
// this procedure is invoked by default (if it exist) on startup

Public p_display
* public PL_FLEX_RE, PL_ARCH_RE, P_CURRROOT, P_EDITHLP, P_LANG, P_SCRN, P_MOUSE

PC_SYSTEM :="Array Lookup Research"

* #define mc_pv_is_mouse .F.

// public vars for some sub-calls
P_USER :="SYSTEM"
pDisplay := .F.
P_EDITHLP := .T.

*rddsetdefault("MEDNTX")

*saGlobal = {}

cScreen := space(4000)
cScreen := savescreen(00, 00, 23, 79)

@ 00, 00 CLEAR to maxrow(), maxcol()

/*
@ 10, 00 say "This is a text line to test the command 'scroll(nn,nn, nn,nn, 0)' '(scoll zero line)"
inkey(0)
scroll(10,00, 10,79,0)
  */

// 12-Dec-2014 - test the incremental seek in Tbrowse on an array
IncrArraySeek()

* MedLogout()
* Disconnect


return
// *** end init Procedure  *** //
// *************************** //
//
// ********************************************************************************
EXIT Procedure Final()
// this procedure is invoked by defaulton exit  (if it exist)

// use for application cleanup

RestScreen(00, 00, 23, 79, cScreen)

return
// *** end EXIT PROCEDURE  *** //
// *************************** //


Function LoadArray (aArray,  nMode)
// ************************************************************
// just load the array
// synopsis: if LoadArray(@aMyArray, "N")
//							// everyhting is ok, sortetd by name
//
//					 else
//							// something is wrong (wrong parameter #2)
//					 endif
//
//

local lReturn

lReturn := .T.

begin sequence

	IF nMode == "N" .OR. nMode == "MC" .OR. nMode == "SC"
		lReturn := .T.

	else
		lReturn := .F.
		break

	ENDIF

/*
ldArray1( @aArray )                        // 1-dim array

DO CASE
CASE nMode == "N"
        aArray := ASort (aArray, , , {|x, y| x[1] < y[1] } )

CASE nMode == "MC"
		aArray := ASort (aArray,,, {|x, y| x[1] < y[1] } )

CASE nMode == "SC"
		aArray := ASort (aArray,,, {|x, y| x[2] < y[2] } )

ENDCASE
        // ASORT(aArray,,, { |x, y| x > y })
        // Result: { 5, 4, 3, 2, 1 }

  */

ldArray2( @aArray )                         // 2-dim array

DO CASE
CASE nMode == "N"


    * aArrau[n, 1] = Main-category
    * aArrau[n, 2] = Sub-category
    * aArrau[n, 3] = Description
    aArray := ASort (aArray, , , {|x, y| x[3] < y[3] } )

CASE nMode == "MC"
    aArray := ASort (aArray,,, {|x, y| x[1] < y[1] } )

CASE nMode == "SC"
    aArray := ASort (aArray,,, {|x, y| x[2] < y[2] } )

ENDCASE
        // ASORT(aArray,,, { |x, y| x > y })
        // Result: { 5, 4, 3, 2, 1 }

end sequence

return (lReturn)
// *** end LoadArray *** //
// ********************* //

Function SimHelp(v1, v2, v3)
// Function to simulate the call to FU_HELP through a cde block at the WHEN clause of a get
SetPos(20, 40)
Dispout('I received ..."')
DispOut(v3)
Dispout('"')
inkey(0)
Scroll(20, 40, 20, 79,0)

return .T.
// ******************* //




// ***************************************************************************************
// ***************************************************************************************
// ***************************************************************************************
// copied from Array.PRG (Clipper sample)
/***
*
*  Array.prg
*
*  Sample array handling functions
*
*  Copyright (c) 1990-1995, Computer Associates International Inc.
*  All rights reserved.
*
*  NOTE: Compile with /a /m /n /w
*
*/

#include "Common.ch"
#include "Inkey.ch"


// Maintains the current row of ABrowse()
* STATIC nRow

/***
*
*  ABrowse( <aArray> [,<nTop>][,<nLeft>][,<nBottom>][,<nRight>] ) --> xValue
*
*  Browse a 2-dimensional array using a TBrowse object
*
*  Parameters:
*     aArray  - The 2D array to browse
*     nTop    - Optional line on which to display the top margin of the browse
*     nLeft   - Optional column of the left margin of the browse
*     nRight  - Optional column of the right margin of the browse
*     nBottom - Optional line of the bottom margin of the browse
*
*  Returns: The value of the highlighted array element
*
*/
FUNCTION ABrowse( aArray, nT, nL, nB, nR, aHeader, aWidth )

LOCAL nOldCursor     // Saves current cursor shape
LOCAL nOldNRow       // Saves current row
LOCAL xRet           // Return value (user's selection or NIL)
LOCAL nKey := 0      // Keystroke holder
LOCAL n              // FOR..NEXT counter variable
LOCAL o              // TBrowse object

local cLook          // seek string

// Preserve cursor setting, turn off cursor
nOldCursor := SETCURSOR( 0 )
nKey       := 0      // Keystroke holder

// Preserve static var (just in case), set it to 1
nOldNRow := nRow
nRow     := 1

// Assign defaults for omitted parameters
DEFAULT nT TO 0
DEFAULT nL TO 0
DEFAULT nB TO MAXROW()
DEFAULT nR TO MAXCOL()

nR := IIF( nR+2 > maxcol(), nR := maxcol()-2, nR )
nB := IIF( nB+2 > maxrow(), nB := maxrow()-2, nB )

DISPBOX(nT-1, nL-1, nB+2, nR+1, B_DOUBLE)

// Create the TBrowse object
o := TBrowseNew( nT, nL, nB, nR )

// This skip block just adds to (or subtracts from) nRow
// (see aSkipTest for explanation of that function)
o:skipBlock := { |nSkip| nSkip := ASkipTest( aArray, nRow, nSkip ), ;
                         nRow += nSkip,                             ;
                         nSkip                                      ;
               }

// The go top block sets nRow to 1
o:goTopBlock := { || nRow := 1 }

// The go bottom block sets nRow to the length of the array
o:goBottomBlock := { || nRow := LEN( aArray ) }

// Create column blocks and add TBColumn objects to the TBrowse
// (see ABrowseBlock() below)

/*
FOR n := 1 TO LEN( aArray[1] )
  oC := TBColumnNew( aHeader[n], ABrowseBlock( aArray, n ))
  oC:Picture := "@"
  oC:Width   := 40
  oC:ColSep  := chr(179)
  o:addColumn( oC )

NEXT
      */

// 1st column
  oC := TBColumnNew( aHeader[1], ABrowseBlock( aArray, 3 ))
  oC:Picture := "@"
  oC:Width   := 40
  oC:HeadSep := chr(196)
  oC:ColSep  := chr(194)+chr(179)+chr(194)
  o:addColumn( oC )


// 2nd column (Main-C)
  oC := TBColumnNew( aHeader[2], ABrowseBlock( aArray, 1 ))
  oC:Picture := "@"
  *oC:Width   := 8
  oC:HeadSep := chr(196)
  oC:ColSep  := chr(179)
  o:addColumn( oC )

// 3rd column (Sub-C)
  oC := TBColumnNew( aHeader[3], ABrowseBlock( aArray, 2 ))
  oC:Picture := "@"
  *oC:Width   := 8
  oC:HeadSep := chr(196)
  oC:ColSep  := chr(179)
  o:addColumn( oC )


// ***************************
* o:ColorSpec :=
* o:ColSep    :=
o:HeadSep   := chr(196)
o:HeadSep   := chr(205)+chr(209)+chr(205)
*o:ColSep    := chr(32)+chr(279)
o:FootSep   := chr(205)+chr(207)+chr(205)
o:FootSep   := chr(196)
o:Cargo     := array(2)                        // array row indicator, start value
o:Cargo[1]  := o:Rowcount()                    // number of rows visible on screen
o:Cargo[2]  := 1
o:Cargo     := 1                               // a counter to the current 'row' in array

cLook := ""
// Main key handler loop
DO WHILE ( nKey <> K_ESC ) .AND. ( nKey <> K_RETURN )

   // Stabilize the browse and wait for a keystroke
   o:forceStable()
   nKey := INKEY( 0 )

   IF o:stable
      // Process the directional keys
      IF !TBMoveCursor( o, nKey )
         IF ( nKey == K_ESC )
             SCROLL()
             *nReturnValue := 0
             EXIT

         ELSEIF ( nKey == K_ENTER )
             // [Enter] - return current array pointer
             EXIT

         ENDIF
      ENDIF

      DO CASE
      CASE ( nKey == K_DOWN )
          o:cargo := IIF( (o:Cargo++) > len(aArray), len(aArray), o:Cargo++)
          cLook := ""

      CASE ( nKey == K_UP )
          o:cargo := IIF( (o:Cargo--) == 1, 1, o:Cargo--)
          cLook := ""

      CASE ( nKey == K_PGDN )
         *o:Cargo := IIF( o:Cargo - o:RowCount < 0, 1, o:Cargo - o:RowCount)
         *o:Cargo += int( o:Cargo / o:rowcount) * o:rowcount
         *o:Cargo += o:RowCount
         *o:Cargo += o:rowcount
         o:cargo := IIF( o:Cargo + o:rowcount > len( aArray ), len( aArray ), o:Cargo + o:rowcount)

      CASE ( nKey == K_PGUP )
         *o:Cargo -= o:RowCount
         o:cargo := IIF( (o:Cargo - o:rowcount) < 1, 1, o:Cargo - o:rowcount)

      CASE ( nKey == K_HOME )
         //o:Cargo := ??

      CASE ( nKey == K_END )
         //o:Cargo := ??

      CASE ( nKey == K_CTRL_LEFT)

      CASE ( nKey == K_CTRL_RIGHT)

      CASE ( nKey == K_CTRL_PGUP)
         // go to 1st row of array
         o:Cargo := 1

      CASE ( nKey == K_CTRL_PGDN)
         // go to last row of array
         o:Cargo := len(aArray)

      CASE ( nKey == K_CTRL_HOME)
         o:Cargo := 1

      CASE ( nKey == K_CTRL_END)
         o:Cargo := len(aArray)

      CASE ( nKey >= 32 .AND. nKey <= 126 )
         cLook += chr(nKey)
         nNewPos := ArrayLookup(aArray, cLook)
         nCurrPos := o:Cargo                               // current position

         DO CASE
         CASE nNewPos > nCurrPos
            nSkip := nNewPos - nCurrPos
            For nI := 1 TO nSkip ; o:Down() ; Next
            o:Cargo := nNewpos

         CASE !empty(nNewPos) .and. (nNewPos < nCurrPos)
           nSkip := nCurrPos - nNewPos
           FOR nI := 1 to nSkip ; o:Up() ; NEXT
           o:Cargo := nNewpos

         CASE nNewPos == 0 .and. len(cLook) > 1
           // new seek string not available in array
           cLook := substr( cLook, 1, len(cLook) -1 )

         ENDCASE

      CASE ( nKey == K_BS )
         cLook := substr(cLook, 1, len(cLook) -1 )
         IF empty(cLook)
            o:GoTop()
            o:Cargo := 1
            scroll(01,50, 01, 79,0)
            loop

         ENDIF
         nNewPos := ArrayLookup(aArray, cLook)
         nCurrPos := o:Cargo
         IF nNewPos == 0
            // new position not found, stay where you are
            loop

         else
            IF nNewPos > nCurrPos
               nSkip := nNewPos - nCurrPos
               For n := 1 TO nSkip
                   o:Down()
               Next
               o:Cargo := nNewPos

            elseif nNewPos < nCurrPos
               nSkip := nCurrPos - nNewPos
               FOR nI := 1 TO nSkip
                   o:Up()
               NEXT
               o:Cargo := nNewPos

            ENDIF
         ENDIF
      ENDCASE
      scroll(01,50, 01, 79,0)
      setpos(01, 50)
      IF len(cLook) > 0
          DispOut("[") ; setcolor("N+/W+") ; Dispout(alltrim(cLook)) ; setcolor("W+/N") ; DispOut("]")

      ENDIF
   ENDIF
ENDDO

// Set the return value
xRet := IIF( nKey == K_RETURN, aArray[nRow, 3], NIL )         // disp 3rd column in array (description)
SetPos(nB+1, nL+1)
*Dispout(trim(aArray[nRow, o:ColPos]))
Dispout(trim(xRet))
inkey(0)

// Restore the original cursor setting
SETCURSOR( nOldCursor )

// Restore the static var
nRow := nOldNRow

RETURN (xRet)



// ***********************************************
Function ArrayLookup( a , cLook)
nPos := 0
nLen := len(a)
nPos := aScan( a, {| a | a[3] = cLook})

return nPos
// *** end ArrayLookup *** //
// *********************** //


// *******************************************************
STATIC FUNCTION TBMoveCursor( o, nKey )
LOCAL nFound
STATIC aKeys    := ;
     { K_DOWN    , { | obj | obj:down() }, ;
     K_UP        , { | obj | obj:up() }, ;
     K_PGDN      , { | obj | obj:pageDown() }, ;
     K_PGUP      , { | obj | obj:pageUp() }, ;
     K_CTRL_PGUP , { | obj | obj:goTop() }, ;
     K_CTRL_PGDN , { | obj | obj:goBottom() }, ;
     K_RIGHT     , { | obj | obj:right() }, ;
     K_LEFT      , { | obj | obj:left() }, ;
     K_HOME      , { | obj | obj:home() }, ;
     K_END       , { | obj | obj:end() }, ;
     K_CTRL_LEFT , { | obj | obj:panLeft() }, ;
     K_CTRL_RIGHT, { | obj | obj:panRight() }, ;
     K_CTRL_HOME , { | obj | obj:panHome() }, ;
     K_CTRL_END  , { | obj | obj:panEnd() } }

nFound := ASCAN( aKeys, nKey )
IF ( nFound != 0 )
  EVAL( aKeys[ ++ nFound ], o )
ENDIF
RETURN ( nFound != 0 )
// *** end func TBMoveCursor *** //
// ***************************** //



/***
*
*  ABrowseBlock( <aArray>, <nIndex> ) --> bColumnBlock
*
*  Create and return a get/set block for <aArray>[nRow, <nIndex>]
*
*  Parameters:
*     aArray - The array for which the code block is to be created
*     nIndex - The index into aArray for the code block creation
*
*  This function works by returning a block that refers
*  to local variables <a> and <x> (the parameters). In
*  version 5.01 these local variables are preserved for
*  use by the block even after the function has returned.
*  The result is that each call to ABrowseBlock() returns
*  a block which has the passed values of <a> and <x> "bound"
*  to it for later use. The block defined here also refers to
*  the static variable nRow, used by ABrowse() to track the
*  array's "current row" while browsing.
*
*/
STATIC FUNCTION ABrowseBlock( a, x )

   RETURN ( {|p| IF( PCOUNT() == 0, a[nRow, x], a[nRow, x] := p ) } )



/***
*
*  ASkipTest( <aArray>, <nCurrent>, <nSkip> ) --> nSkipsPossible
*
*  Given array <aArray> whose "current" row is <nCurrent>, determine
*  whether it is possible to "skip" forward or backward by
*  <nSkip> rows
*
*  Parameters:
*     aArray   - The array on which to perform the "skip test"
*     nCurrent - The currently selected array element
*     nSkip    - The requested number of rows to skip, negative numbers
*                meaning to "skip" backwards
*
*  Returns the number of skips actually possible
*
*/
STATIC FUNCTION ASkipTest( a, nCurrent, nSkip )

   IF ( nCurrent + nSkip < 1 )

      // Would skip past the top...
      RETURN ( -nCurrent + 1 )

   ELSEIF ( nCurrent + nSkip > LEN( a ) )

      // Would skip past the bottom...
      RETURN ( LEN(a) - nCurrent )

   END

   // No problem
   RETURN ( nSkip )



/***
*
*  ABlock( <cName>, <nSubx> ) --> bABlock
*
*  Create a get/set block for the specified array element
*
*  Parameters:
*     cName - The name of the array variable for which the code block is
*             to be created
*     nSubx - The index into the array which determines the array element
*             to use
*
*  NOTE:
*     cName must be the name of a variable that is visible
*     in macros (i.e. not a LOCAL or STATIC variable). Also, the
*     variable must be visible anywhere where the block is to be
*     used.
*
*     ABlock() may be used to make blocks for a nested array
*     by including a subscript expression as part of cName:
*
*       // to make a set/get block for a[i]
*       b := ABlock( "a", i )
*
*       // to make a set/get block for a[i][j]
*       b :=- ABlock( "a[i]", j )
*
*     This function is provided for compatibility with the
*     version 5.00 Array.prg. See the ABrowseBlock() function
*     (above) for a method of "binding" an array to a block
*     without using a macro.
*
*/
FUNCTION ABlock( cName, nSubx )

LOCAL cAXpr

   cAXpr := cName + "[" + LTRIM( STR( nSubx )) + "]"

   RETURN &( "{ |p| IF(PCOUNT()==0, " + cAXpr + "," + cAXpr + ":=p) }" )




/***
*           Array utility functions
*/


/***
*
*  AMax( <aArray> ) --> nPos
*
*  Search aArray for the position of its highest numerical value
*
*  Parameter:
*     aArray - The array to be "searched" for the highest value
*
*  Returns: The subscript of the array element with the highest value or
*           zero if an error occurred
*
*/
FUNCTION AMax( aArray )

   LOCAL nLen        // The length of aArray
   LOCAL nPos        // The position of the highest element
   LOCAL nLastExpr   // The value of the last element
   LOCAL nElement    // Loop counter variable

   DO CASE

   // The argument is not an array
   CASE VALTYPE( aArray ) <> "A"
      nPos := 0

   // The array has no elements
   CASE EMPTY( aArray )
      nPos := 0

   // If we made it this far, assume the variable's ok
   OTHERWISE

      nLen      := LEN( aArray )
      nPos      := 1
      nLastExpr := aArray[nPos]
      FOR nElement := 2 TO nLen
         IF ( aArray[nElement] > nLastExpr )

            // Make this element the current maximum and assign it to
            // nLastExpr for future comparisons
            nPos := nElement
            nLastExpr := aArray[nElement]

         ENDIF
      NEXT

   ENDCASE

   RETURN ( nPos )


/***
*
*  AMin( <aArray> ) --> nPos
*
*  Search aArray for the position of its lowest numerical value
*
*  Parameter:
*     aArray - The array to be "searched" for the minimum value
*
*  Returns: The subscript of the array element with the minimum value or
*           zero if an error occurred
*
*/
FUNCTION AMin( aArray )

   LOCAL nLen        // The length of aArray
   LOCAL nPos        // The position of the highest element
   LOCAL nLastExpr   // The value of the last element
   LOCAL nElement    // Loop counter variable

   DO CASE

   // Argument is not an array
   CASE VALTYPE( aArray ) <> "A"
      nPos := 0

   // Array is empty
   CASE EMPTY( aArray )
      nPos := 0

   // Assume we're ok
   OTHERWISE

      nLen      := LEN( aArray )
      nPos      := 1
      nLastExpr := aArray[nPos]
      FOR nElement := 2 TO nLen

         // If this element is less than previous elements, assign it as
         // the current minimum
         IF aArray[nElement] < nLastExpr
            nPos := nElement
            nLastExpr := aArray[nElement]
         ENDIF

      NEXT

   ENDCASE

   RETURN ( nPos )


/***
*
*  AComp( <aArray>, <bComp> [, <nStart>] [, <nStop>] ) --> xElementValue
*
*  Compare all elements of aArray using the bComp block from nStart to
*  nStop (if specified, otherwise entire array) and return the result.
*
*  Parameters:
*     aArray - Array to be compared
*     bComp  - Code block containing the comparison expression
*     nStart - Optional starting element to compare
*     nStop  - Optional ending element to compare
*
*  NOTE: Several sample blocks are provided in Array.ch.
*
*/
FUNCTION AComp( aArray, bComp, nStart, nStop )

   LOCAL xVal := aArray[1]    // Value of the element matching the condition

   AEVAL(                                                               ;
          aArray,                                                       ;
          {|x| xVal := IF( EVAL( bComp, x, xVal ), x, xVal ) },         ;
          nStart,                                                       ;
          nStop                                                         ;
        )

   RETURN( xVal )


/***
*
*  Dimensions( <aArray> ) --> aDims
*
*  Calculate the dimensions of a multi-dimensional array
*
*  Parameter:
*     aArray - Array to be calculated
*
*  Returns: An array of numeric values describing the dimensions of aArray
*
*  NOTE: Assumes aArray has uniform dimensions (i.e. is not a ragged array)
*
*/
FUNCTION Dimensions( aArray )

   LOCAL aDims := {}    // Array to contain the dimensions

   // We keep "traversing" the array until the first element is NOT an array
   DO WHILE ( VALTYPE( aArray ) == "A" )

      // Add the size of this dimension to aDims and use this array's first
      // element as the array for the next iteration of the loop
      AADD( aDims, LEN(aArray) )
      aArray := aArray[1]

   ENDDO

   RETURN ( aDims )

// ***********************************************************************
Procedure IncrArraySeek()
local aArray
local nTop, nLeft, nBottom, nRight

Local aHeader

aHeader := {"Description"}				// now just a 1ele array
aHeader := {"Description", "Main-C", "Sub-C"}        // now just a 1ele array

aArray := {}

nTop    := 05
nLeft   := 03
nBottom := 18
nRight  := 55

If LoadArray(@aArray, "N")
	ABrowse (aArray, nTop, nLeft, nBottom, nRight, aHeader)

else
  Alert('some error while executing "Func ABrowse()"')

endif

return
// *** end IncrArraySeek *** //
// ************************* //


#INCLUDE "ArrLook.CH"
