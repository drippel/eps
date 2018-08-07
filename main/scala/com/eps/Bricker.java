package com.eps;

import java.util.function.Function;

public class Bricker {

    interface mbfi {
        public boolean apply( int small, int bin, int smallSize, int bigSize, int goal );
    }

    public static final void main( String[] args ){
        System.out.println("bricker...");

        System.out.println(makeBricks(3, 1, 1, 5, 8));
        System.out.println(makeBricks(3, 1, 1, 5, 9));
        System.out.println(makeBricks(3, 2, 1, 5, 9));
        System.out.println(makeBricks(3, 2, 1, 5, 10));

        mbfi mbf = new mbfi() {
            public boolean apply( int small, int big, int smallSize, int bigSize, int goal ) {

            if( goal == 0 ){
                return true;
            }
            else {

                if( big > 0 && goal >= bigSize ){
                    return apply( small, bigSize - 1, smallSize, bigSize, goal - bigSize );
                }
                else if( small > 0 && goal >= smallSize ) {
                    return apply( small - 1, big, smallSize, bigSize, goal - smallSize );
                }
                else {
                    return false;
                }
            }
            }
        };
        System.out.println(mbf.apply(3, 1, 1, 5, 8));
        System.out.println(mbf.apply(3, 1, 1, 5, 9));
        System.out.println(mbf.apply(3, 2, 1, 5, 9));
        System.out.println(mbf.apply(3, 2, 1, 5, 10));

    }

    public static boolean makeBricks( int small, int big, int smallSize, int bigSize, int goal ) {

        if( goal == 0 ){
            return true;
        }
        else {

            if( big > 0 && goal >= bigSize ){
                return makeBricks( small, bigSize - 1, smallSize, bigSize, goal - bigSize );
            }
            else if( small > 0 && goal >= smallSize ) {
                return makeBricks( small - 1, big, smallSize, bigSize, goal - smallSize );
            }
            else {
                return false;
            }
        }
    }

}
