package com.weizilla.adventofcode.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Utils {
    private static final Pattern RUN_DAY_PATTERN = Pattern.compile("y(\\d{4}).Day(\\d{2})");

    public static RunDay getRunDay() {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        for (StackTraceElement e : stack) {
            Matcher matcher = RUN_DAY_PATTERN.matcher(e.getClassName());
            if (matcher.find()) {
                int year = Integer.parseInt(matcher.group(1));
                int day = Integer.parseInt(matcher.group(2));
                return new RunDay(year, day);
            }
        }

        throw new RuntimeException("Did not find run year day");
    }
}
