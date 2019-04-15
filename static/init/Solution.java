import java.util.*;
import java.io.*;
import java.math.*;

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
class Solution {

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        int L = in.nextInt();
        int H = in.nextInt();
        if (in.hasNextLine()) {
            in.nextLine();
        }
        String T = in.nextLine().toUpperCase();
        
        
        String str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?";
        System.err.println(T);
        for (int i = 0; i < H; i++) {
            String ROW = in.nextLine();
           
            for(char c:T.toCharArray()){
                System.err.println(c);
                int index=26;
                if(c>='A'&&c<='Z'){
                    index=str.indexOf(c);
                }
                System.err.println(index);
                System.out.print(ROW.substring(index*L,index*L+L));  
            }
            System.out.println();
        }
       
        // Write an action using System.out.println()
        
        // To debug: System.err.println("Debug messages...");

    }
}