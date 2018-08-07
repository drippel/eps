package com.eps;

import java.text.SimpleDateFormat;
import java.util.Date;

public class JTest {

    public static void main( String[] args ){
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
        System.out.println( sdf.format( new Date() ));
    }
}
