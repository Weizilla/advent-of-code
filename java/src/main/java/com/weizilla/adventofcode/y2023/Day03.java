package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.Matrix;
import com.weizilla.adventofcode.utils.Point;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.weizilla.adventofcode.utils.InputReader.readMatrix;

public class Day03 extends Day {
    @Override
    public Object part1() {
        var matrix = readMatrix();
        var nums = new ArrayList<Integer>();

        for (int y = matrix.getMinY(); y <= matrix.getMaxY(); y++) {
            int num = 0;
            boolean isNum = false;
            boolean isAdjSymbol = false;

            for (int x = matrix.getMinX(); x <= matrix.getMaxX(); x++) {
                String value = matrix.get(x, y);
                if (Character.isDigit(value.charAt(0))) {
                    num = num * 10 + Integer.parseInt(value);
                    isNum = true;
                    isAdjSymbol = isAdjSymbol || isAdjSymbol(x, y, matrix);
                } else {
                    if (isNum) {
                        if (isAdjSymbol) {
                            nums.add(num);
                        }
                        isNum = false;
                        isAdjSymbol = false;
                        num = 0;
                    }
                }
            }
            if (isNum && isAdjSymbol) {
                nums.add(num);
            }
        }

        return nums.stream().mapToInt(Integer::valueOf).sum();
    }

    @Override
    public Object part2() {
        var matrix = readMatrix();
        var stars = stars(matrix);
        var starsWithNums = new HashMap<Point, List<Integer>>();

        for (int y = matrix.getMinY(); y <= matrix.getMaxY(); y++) {
            int num = 0;
            boolean isNum = false;
            var adjStars = new HashSet<Point>();

            for (int x = matrix.getMinX(); x <= matrix.getMaxX(); x++) {
                String value = matrix.get(x, y);
                if (Character.isDigit(value.charAt(0))) {
                    num = num * 10 + Integer.parseInt(value);
                    isNum = true;

                    for (int xx = -1; xx <= 1; xx++) {
                        for (int yy = -1; yy <= 1; yy++) {
                            if (!(xx == 0 && yy == 0)) {
                                var p = new Point(x + xx, y + yy);
                                if (stars.contains(p)){
                                    adjStars.add(p);
                                }
                            }
                        }
                    }
                } else {
                    if (isNum) {
                        // do stuff with found stars
                        for (Point p : adjStars) {
                            starsWithNums.computeIfAbsent(p, k -> new ArrayList<>()).add(num);
                        }

                        isNum = false;
                        num = 0;
                        adjStars.clear();
                    }
                }
            }
            if (isNum) {
                // do stuff with found stars
                for (Point p : adjStars) {
                    starsWithNums.computeIfAbsent(p, k -> new ArrayList<>()).add(num);
                }
            }
        }

        var sum = starsWithNums.values().stream()
            .filter(n -> n.size() == 2)
            .mapToInt(n -> n.getFirst() * n.get(1))
            .sum();

        return sum;
    }

    private static Set<Point> stars(Matrix matrix) {
        var stars = new HashSet<Point>();

        matrix.iterate((x, y, value) -> {
            if (value.equals("*")) {
                stars.add(new Point(x, y));
            }
        });

        return stars;
    }

    private static boolean isAdjSymbol(int x, int y, Matrix matrix) {
        return isSymbol(matrix.get(x - 1, y - 1))
            || isSymbol(matrix.get(x, y - 1))
            || isSymbol(matrix.get(x + 1, y - 1))
            || isSymbol(matrix.get(x - 1, y))
            || isSymbol(matrix.get(x + 1, y))
            || isSymbol(matrix.get(x - 1, y + 1))
            || isSymbol(matrix.get(x, y + 1))
            || isSymbol(matrix.get(x + 1, y + 1));
    }

    private static boolean isSymbol(String value) {
        return value != null && !Character.isDigit(value.charAt(0)) && ! value.equals(".");
    }

    private static boolean isStar(String value) {
        return "*".equals(value);
    }
}
