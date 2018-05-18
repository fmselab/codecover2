/* Generated By:JavaCC: Do not edit this line. CoverageLogParserTokenManager.java */
package org.codecover.instrumentation.measurement.parser;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.instrumentation.measurement.NullCoverageLog;
import org.codecover.model.utils.StringUtil;
import static org.codecover.instrumentation.measurement.MeasurementConstants.CHARSET;

public class CoverageLogParserTokenManager implements CoverageLogParserConstants
{
  public  java.io.PrintStream debugStream = System.out;
  public  void setDebugStream(java.io.PrintStream ds) { debugStream = ds; }
private final int jjStopStringLiteralDfa_0(int pos, long active0)
{
   switch (pos)
   {
      case 0:
         if ((active0 & 0x1d0L) != 0L)
         {
            jjmatchedKind = 9;
            return 9;
         }
         return -1;
      case 1:
         if ((active0 & 0x1d0L) != 0L)
         {
            jjmatchedKind = 9;
            jjmatchedPos = 1;
            return 9;
         }
         return -1;
      case 2:
         if ((active0 & 0x1d0L) != 0L)
         {
            jjmatchedKind = 9;
            jjmatchedPos = 2;
            return 9;
         }
         return -1;
      case 3:
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         if ((active0 & 0x1c0L) != 0L)
         {
            jjmatchedKind = 9;
            jjmatchedPos = 3;
            return 9;
         }
         return -1;
      case 4:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         if ((active0 & 0x180L) != 0L)
         {
            jjmatchedKind = 9;
            jjmatchedPos = 4;
            return 9;
         }
         return -1;
      case 5:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 6:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 7:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 8:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 9:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 10:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 11:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 12:
         if ((active0 & 0x180L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         if ((active0 & 0x10L) != 0L)
         {
            if (jjmatchedPos < 2)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 2;
            }
            return -1;
         }
         return -1;
      case 13:
         if ((active0 & 0x80L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 14:
         if ((active0 & 0x80L) != 0L)
         {
            if (jjmatchedPos < 4)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 4;
            }
            return -1;
         }
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 15:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 16:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 17:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 18:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 19:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      case 20:
         if ((active0 & 0x40L) != 0L)
         {
            if (jjmatchedPos < 3)
            {
               jjmatchedKind = 9;
               jjmatchedPos = 3;
            }
            return -1;
         }
         return -1;
      default :
         return -1;
   }
}
private final int jjStartNfa_0(int pos, long active0)
{
   return jjMoveNfa_0(jjStopStringLiteralDfa_0(pos, active0), pos + 1);
}
private final int jjStopAtPos(int pos, int kind)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   return pos + 1;
}
private final int jjStartNfaWithStates_0(int pos, int kind, int state)
{
   jjmatchedKind = kind;
   jjmatchedPos = pos;
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) { return pos + 1; }
   return jjMoveNfa_0(state, pos + 1);
}
private final int jjMoveStringLiteralDfa0_0()
{
   switch(curChar)
   {
      case 32:
         return jjStopAtPos(0, 2);
      case 45:
         return jjStopAtPos(0, 5);
      case 69:
         return jjMoveStringLiteralDfa1_0(0x10L);
      case 83:
         return jjMoveStringLiteralDfa1_0(0x180L);
      case 84:
         return jjMoveStringLiteralDfa1_0(0x40L);
      default :
         return jjMoveNfa_0(5, 0);
   }
}
private final int jjMoveStringLiteralDfa1_0(long active0)
{
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(0, active0);
      return 1;
   }
   switch(curChar)
   {
      case 69:
         return jjMoveStringLiteralDfa2_0(active0, 0x40L);
      case 78:
         return jjMoveStringLiteralDfa2_0(active0, 0x10L);
      case 84:
         return jjMoveStringLiteralDfa2_0(active0, 0x180L);
      default :
         break;
   }
   return jjStartNfa_0(0, active0);
}
private final int jjMoveStringLiteralDfa2_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(0, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(1, active0);
      return 2;
   }
   switch(curChar)
   {
      case 65:
         return jjMoveStringLiteralDfa3_0(active0, 0x180L);
      case 68:
         return jjMoveStringLiteralDfa3_0(active0, 0x10L);
      case 83:
         return jjMoveStringLiteralDfa3_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(1, active0);
}
private final int jjMoveStringLiteralDfa3_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(1, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(2, active0);
      return 3;
   }
   switch(curChar)
   {
      case 82:
         return jjMoveStringLiteralDfa4_0(active0, 0x180L);
      case 84:
         return jjMoveStringLiteralDfa4_0(active0, 0x40L);
      case 95:
         return jjMoveStringLiteralDfa4_0(active0, 0x10L);
      default :
         break;
   }
   return jjStartNfa_0(2, active0);
}
private final int jjMoveStringLiteralDfa4_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(2, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(3, active0);
      return 4;
   }
   switch(curChar)
   {
      case 84:
         return jjMoveStringLiteralDfa5_0(active0, 0x190L);
      case 95:
         return jjMoveStringLiteralDfa5_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(3, active0);
}
private final int jjMoveStringLiteralDfa5_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(3, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(4, active0);
      return 5;
   }
   switch(curChar)
   {
      case 69:
         return jjMoveStringLiteralDfa6_0(active0, 0x10L);
      case 83:
         return jjMoveStringLiteralDfa6_0(active0, 0x40L);
      case 95:
         return jjMoveStringLiteralDfa6_0(active0, 0x180L);
      default :
         break;
   }
   return jjStartNfa_0(4, active0);
}
private final int jjMoveStringLiteralDfa6_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(4, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(5, active0);
      return 6;
   }
   switch(curChar)
   {
      case 69:
         return jjMoveStringLiteralDfa7_0(active0, 0x40L);
      case 83:
         return jjMoveStringLiteralDfa7_0(active0, 0x110L);
      case 84:
         return jjMoveStringLiteralDfa7_0(active0, 0x80L);
      default :
         break;
   }
   return jjStartNfa_0(5, active0);
}
private final int jjMoveStringLiteralDfa7_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(5, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(6, active0);
      return 7;
   }
   switch(curChar)
   {
      case 69:
         return jjMoveStringLiteralDfa8_0(active0, 0x180L);
      case 83:
         return jjMoveStringLiteralDfa8_0(active0, 0x40L);
      case 84:
         return jjMoveStringLiteralDfa8_0(active0, 0x10L);
      default :
         break;
   }
   return jjStartNfa_0(6, active0);
}
private final int jjMoveStringLiteralDfa8_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(6, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(7, active0);
      return 8;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa9_0(active0, 0x100L);
      case 83:
         return jjMoveStringLiteralDfa9_0(active0, 0xc0L);
      case 95:
         return jjMoveStringLiteralDfa9_0(active0, 0x10L);
      default :
         break;
   }
   return jjStartNfa_0(7, active0);
}
private final int jjMoveStringLiteralDfa9_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(7, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(8, active0);
      return 9;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa10_0(active0, 0x10L);
      case 73:
         return jjMoveStringLiteralDfa10_0(active0, 0x40L);
      case 84:
         return jjMoveStringLiteralDfa10_0(active0, 0x180L);
      default :
         break;
   }
   return jjStartNfa_0(8, active0);
}
private final int jjMoveStringLiteralDfa10_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(8, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(9, active0);
      return 10;
   }
   switch(curChar)
   {
      case 65:
         return jjMoveStringLiteralDfa11_0(active0, 0x10L);
      case 73:
         return jjMoveStringLiteralDfa11_0(active0, 0x100L);
      case 79:
         return jjMoveStringLiteralDfa11_0(active0, 0x40L);
      case 95:
         return jjMoveStringLiteralDfa11_0(active0, 0x80L);
      default :
         break;
   }
   return jjStartNfa_0(9, active0);
}
private final int jjMoveStringLiteralDfa11_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(9, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(10, active0);
      return 11;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa12_0(active0, 0x80L);
      case 78:
         return jjMoveStringLiteralDfa12_0(active0, 0x40L);
      case 79:
         return jjMoveStringLiteralDfa12_0(active0, 0x100L);
      case 83:
         return jjMoveStringLiteralDfa12_0(active0, 0x10L);
      default :
         break;
   }
   return jjStartNfa_0(10, active0);
}
private final int jjMoveStringLiteralDfa12_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(10, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(11, active0);
      return 12;
   }
   switch(curChar)
   {
      case 65:
         return jjMoveStringLiteralDfa13_0(active0, 0x80L);
      case 69:
         if ((active0 & 0x10L) != 0L)
            return jjStopAtPos(12, 4);
         break;
      case 78:
         if ((active0 & 0x100L) != 0L)
            return jjStopAtPos(12, 8);
         break;
      case 95:
         return jjMoveStringLiteralDfa13_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(11, active0);
}
private final int jjMoveStringLiteralDfa13_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(11, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(12, active0);
      return 13;
   }
   switch(curChar)
   {
      case 67:
         return jjMoveStringLiteralDfa14_0(active0, 0x40L);
      case 83:
         return jjMoveStringLiteralDfa14_0(active0, 0x80L);
      default :
         break;
   }
   return jjStartNfa_0(12, active0);
}
private final int jjMoveStringLiteralDfa14_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(12, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(13, active0);
      return 14;
   }
   switch(curChar)
   {
      case 69:
         if ((active0 & 0x80L) != 0L)
            return jjStopAtPos(14, 7);
         break;
      case 79:
         return jjMoveStringLiteralDfa15_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(13, active0);
}
private final int jjMoveStringLiteralDfa15_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(13, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(14, active0);
      return 15;
   }
   switch(curChar)
   {
      case 78:
         return jjMoveStringLiteralDfa16_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(14, active0);
}
private final int jjMoveStringLiteralDfa16_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(14, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(15, active0);
      return 16;
   }
   switch(curChar)
   {
      case 84:
         return jjMoveStringLiteralDfa17_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(15, active0);
}
private final int jjMoveStringLiteralDfa17_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(15, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(16, active0);
      return 17;
   }
   switch(curChar)
   {
      case 65:
         return jjMoveStringLiteralDfa18_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(16, active0);
}
private final int jjMoveStringLiteralDfa18_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(16, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(17, active0);
      return 18;
   }
   switch(curChar)
   {
      case 73:
         return jjMoveStringLiteralDfa19_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(17, active0);
}
private final int jjMoveStringLiteralDfa19_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(17, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(18, active0);
      return 19;
   }
   switch(curChar)
   {
      case 78:
         return jjMoveStringLiteralDfa20_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(18, active0);
}
private final int jjMoveStringLiteralDfa20_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(18, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(19, active0);
      return 20;
   }
   switch(curChar)
   {
      case 69:
         return jjMoveStringLiteralDfa21_0(active0, 0x40L);
      default :
         break;
   }
   return jjStartNfa_0(19, active0);
}
private final int jjMoveStringLiteralDfa21_0(long old0, long active0)
{
   if (((active0 &= old0)) == 0L)
      return jjStartNfa_0(19, old0); 
   try { curChar = input_stream.readChar(); }
   catch(java.io.IOException e) {
      jjStopStringLiteralDfa_0(20, active0);
      return 21;
   }
   switch(curChar)
   {
      case 82:
         if ((active0 & 0x40L) != 0L)
            return jjStopAtPos(21, 6);
         break;
      default :
         break;
   }
   return jjStartNfa_0(20, active0);
}
private final void jjCheckNAdd(int state)
{
   if (jjrounds[state] != jjround)
   {
      jjstateSet[jjnewStateCnt++] = state;
      jjrounds[state] = jjround;
   }
}
private final void jjAddStates(int start, int end)
{
   do {
      jjstateSet[jjnewStateCnt++] = jjnextStates[start];
   } while (start++ != end);
}
private final void jjCheckNAddTwoStates(int state1, int state2)
{
   jjCheckNAdd(state1);
   jjCheckNAdd(state2);
}
private final void jjCheckNAddStates(int start, int end)
{
   do {
      jjCheckNAdd(jjnextStates[start]);
   } while (start++ != end);
}
private final void jjCheckNAddStates(int start)
{
   jjCheckNAdd(jjnextStates[start]);
   jjCheckNAdd(jjnextStates[start + 1]);
}
static final long[] jjbitVec0 = {
   0xfffffffffffffffeL, 0xffffffffffffffffL, 0xffffffffffffffffL, 0xffffffffffffffffL
};
static final long[] jjbitVec2 = {
   0x0L, 0x0L, 0xffffffffffffffffL, 0xffffffffffffffffL
};
private final int jjMoveNfa_0(int startState, int curPos)
{
   int[] nextStates;
   int startsAt = 0;
   jjnewStateCnt = 16;
   int i = 1;
   jjstateSet[0] = startState;
   int j, kind = 0x7fffffff;
   for (;;)
   {
      if (++jjround == 0x7fffffff)
         ReInitRounds();
      if (curChar < 64)
      {
         long l = 1L << curChar;
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 5:
                  if ((0x3ff000000000000L & l) != 0L)
                  {
                     if (kind > 10)
                        kind = 10;
                     jjCheckNAdd(10);
                  }
                  else if ((0x2400L & l) != 0L)
                  {
                     if (kind > 3)
                        kind = 3;
                  }
                  else if (curChar == 34)
                     jjCheckNAddStates(0, 2);
                  else if (curChar == 47)
                     jjstateSet[jjnewStateCnt++] = 0;
                  if (curChar == 13)
                     jjstateSet[jjnewStateCnt++] = 7;
                  break;
               case 0:
                  if (curChar != 47)
                     break;
                  if (kind > 1)
                     kind = 1;
                  jjCheckNAddStates(3, 5);
                  break;
               case 1:
                  if ((0xffffffffffffdbffL & l) == 0L)
                     break;
                  if (kind > 1)
                     kind = 1;
                  jjCheckNAddStates(3, 5);
                  break;
               case 2:
                  if ((0x2400L & l) != 0L && kind > 1)
                     kind = 1;
                  break;
               case 3:
                  if (curChar == 10 && kind > 1)
                     kind = 1;
                  break;
               case 4:
                  if (curChar == 13)
                     jjstateSet[jjnewStateCnt++] = 3;
                  break;
               case 6:
                  if ((0x2400L & l) != 0L && kind > 3)
                     kind = 3;
                  break;
               case 7:
                  if (curChar == 10 && kind > 3)
                     kind = 3;
                  break;
               case 8:
                  if (curChar == 13)
                     jjstateSet[jjnewStateCnt++] = 7;
                  break;
               case 10:
                  if ((0x3ff000000000000L & l) == 0L)
                     break;
                  if (kind > 10)
                     kind = 10;
                  jjCheckNAdd(10);
                  break;
               case 11:
                  if (curChar == 34)
                     jjCheckNAddStates(0, 2);
                  break;
               case 12:
                  if ((0xfffffffbffffdbffL & l) != 0L)
                     jjCheckNAddStates(0, 2);
                  break;
               case 14:
                  if ((0x8400000000L & l) != 0L)
                     jjCheckNAddStates(0, 2);
                  break;
               case 15:
                  if (curChar == 34 && kind > 11)
                     kind = 11;
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else if (curChar < 128)
      {
         long l = 1L << (curChar & 077);
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 5:
               case 9:
                  if ((0x7fffffe07fffffeL & l) == 0L)
                     break;
                  if (kind > 9)
                     kind = 9;
                  jjCheckNAdd(9);
                  break;
               case 1:
                  if (kind > 1)
                     kind = 1;
                  jjAddStates(3, 5);
                  break;
               case 12:
                  if ((0xffffffffefffffffL & l) != 0L)
                     jjCheckNAddStates(0, 2);
                  break;
               case 13:
                  if (curChar == 92)
                     jjstateSet[jjnewStateCnt++] = 14;
                  break;
               case 14:
                  if ((0x14404410000000L & l) != 0L)
                     jjCheckNAddStates(0, 2);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      else
      {
         int hiByte = (int)(curChar >> 8);
         int i1 = hiByte >> 6;
         long l1 = 1L << (hiByte & 077);
         int i2 = (curChar & 0xff) >> 6;
         long l2 = 1L << (curChar & 077);
         MatchLoop: do
         {
            switch(jjstateSet[--i])
            {
               case 1:
                  if (!jjCanMove_0(hiByte, i1, i2, l1, l2))
                     break;
                  if (kind > 1)
                     kind = 1;
                  jjAddStates(3, 5);
                  break;
               case 12:
                  if (jjCanMove_0(hiByte, i1, i2, l1, l2))
                     jjAddStates(0, 2);
                  break;
               default : break;
            }
         } while(i != startsAt);
      }
      if (kind != 0x7fffffff)
      {
         jjmatchedKind = kind;
         jjmatchedPos = curPos;
         kind = 0x7fffffff;
      }
      ++curPos;
      if ((i = jjnewStateCnt) == (startsAt = 16 - (jjnewStateCnt = startsAt)))
         return curPos;
      try { curChar = input_stream.readChar(); }
      catch(java.io.IOException e) { return curPos; }
   }
}
static final int[] jjnextStates = {
   12, 13, 15, 1, 2, 4, 
};
private static final boolean jjCanMove_0(int hiByte, int i1, int i2, long l1, long l2)
{
   switch(hiByte)
   {
      case 0:
         return ((jjbitVec2[i2] & l2) != 0L);
      default : 
         if ((jjbitVec0[i1] & l1) != 0L)
            return true;
         return false;
   }
}
public static final String[] jjstrLiteralImages = {
"", null, "\40", null, "\105\116\104\137\124\105\123\124\137\103\101\123\105", 
"\55", 
"\124\105\123\124\137\123\105\123\123\111\117\116\137\103\117\116\124\101\111\116\105\122", "\123\124\101\122\124\137\124\105\123\124\137\103\101\123\105", 
"\123\124\101\122\124\137\123\105\103\124\111\117\116", null, null, null, };
public static final String[] lexStateNames = {
   "DEFAULT", 
};
static final long[] jjtoToken = {
   0xffdL, 
};
static final long[] jjtoSkip = {
   0x2L, 
};
static final long[] jjtoSpecial = {
   0x2L, 
};
protected SimpleCharStream input_stream;
private final int[] jjrounds = new int[16];
private final int[] jjstateSet = new int[32];
protected char curChar;
public CoverageLogParserTokenManager(SimpleCharStream stream){
   if (SimpleCharStream.staticFlag)
      throw new Error("ERROR: Cannot use a static CharStream class with a non-static lexical analyzer.");
   input_stream = stream;
}
public CoverageLogParserTokenManager(SimpleCharStream stream, int lexState){
   this(stream);
   SwitchTo(lexState);
}
public void ReInit(SimpleCharStream stream)
{
   jjmatchedPos = jjnewStateCnt = 0;
   curLexState = defaultLexState;
   input_stream = stream;
   ReInitRounds();
}
private final void ReInitRounds()
{
   int i;
   jjround = 0x80000001;
   for (i = 16; i-- > 0;)
      jjrounds[i] = 0x80000000;
}
public void ReInit(SimpleCharStream stream, int lexState)
{
   ReInit(stream);
   SwitchTo(lexState);
}
public void SwitchTo(int lexState)
{
   if (lexState >= 1 || lexState < 0)
      throw new TokenMgrError("Error: Ignoring invalid lexical state : " + lexState + ". State unchanged.", TokenMgrError.INVALID_LEXICAL_STATE);
   else
      curLexState = lexState;
}

protected Token jjFillToken()
{
   Token t = Token.newToken(jjmatchedKind);
   t.kind = jjmatchedKind;
   String im = jjstrLiteralImages[jjmatchedKind];
   t.image = (im == null) ? input_stream.GetImage() : im;
   t.beginLine = input_stream.getBeginLine();
   t.beginColumn = input_stream.getBeginColumn();
   t.endLine = input_stream.getEndLine();
   t.endColumn = input_stream.getEndColumn();
   return t;
}

int curLexState = 0;
int defaultLexState = 0;
int jjnewStateCnt;
int jjround;
int jjmatchedPos;
int jjmatchedKind;

public Token getNextToken() 
{
  int kind;
  Token specialToken = null;
  Token matchedToken;
  int curPos = 0;

  EOFLoop :
  for (;;)
  {   
   try   
   {     
      curChar = input_stream.BeginToken();
   }     
   catch(java.io.IOException e)
   {        
      jjmatchedKind = 0;
      matchedToken = jjFillToken();
      matchedToken.specialToken = specialToken;
      return matchedToken;
   }

   jjmatchedKind = 0x7fffffff;
   jjmatchedPos = 0;
   curPos = jjMoveStringLiteralDfa0_0();
   if (jjmatchedKind != 0x7fffffff)
   {
      if (jjmatchedPos + 1 < curPos)
         input_stream.backup(curPos - jjmatchedPos - 1);
      if ((jjtoToken[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
      {
         matchedToken = jjFillToken();
         matchedToken.specialToken = specialToken;
         return matchedToken;
      }
      else
      {
         if ((jjtoSpecial[jjmatchedKind >> 6] & (1L << (jjmatchedKind & 077))) != 0L)
         {
            matchedToken = jjFillToken();
            if (specialToken == null)
               specialToken = matchedToken;
            else
            {
               matchedToken.specialToken = specialToken;
               specialToken = (specialToken.next = matchedToken);
            }
         }
         continue EOFLoop;
      }
   }
   int error_line = input_stream.getEndLine();
   int error_column = input_stream.getEndColumn();
   String error_after = null;
   boolean EOFSeen = false;
   try { input_stream.readChar(); input_stream.backup(1); }
   catch (java.io.IOException e1) {
      EOFSeen = true;
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
      if (curChar == '\n' || curChar == '\r') {
         error_line++;
         error_column = 0;
      }
      else
         error_column++;
   }
   if (!EOFSeen) {
      input_stream.backup(1);
      error_after = curPos <= 1 ? "" : input_stream.GetImage();
   }
   throw new TokenMgrError(EOFSeen, curLexState, error_line, error_column, error_after, curChar, TokenMgrError.LEXICAL_ERROR);
  }
}

}
