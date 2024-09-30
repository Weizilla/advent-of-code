package com.weizilla.adventofcode.utils;

import java.util.HashMap;
import java.util.Map;

public class Matrix {
    private final Map<Point, String> values;
    private int minX = Integer.MAX_VALUE;
    private int minY = Integer.MAX_VALUE;
    private int maxX = Integer.MIN_VALUE;
    private int maxY = Integer.MIN_VALUE;

    public Matrix() {
        values = new HashMap<>();
    }

    public void put(int x, int y, String value) {
        values.put(new Point(x, y), value);
        minX = Math.min(minX, x);
        minY = Math.min(minY, y);
        maxX = Math.max(maxX, x);
        maxY = Math.max(maxY, y);
    }

    @Override
    public String toString() {
        return "Matrix{" +
            "values=" + values +
            '}';
    }

    public String get(int x, int y) {
        return values.get(new Point(x, y));
    }

    public int getMaxY() {
        if (values.isEmpty()) {
            throw new IllegalStateException("no values stored");
        }

        return maxY;
    }

    public int getMaxX() {
        if (values.isEmpty()) {
            throw new IllegalStateException("no values stored");
        }

        return maxX;
    }

    public int getMinY() {
        if (values.isEmpty()) {
            throw new IllegalStateException("no values stored");
        }

        return minY;
    }

    public int getMinX() {
        if (values.isEmpty()) {
            throw new IllegalStateException("no values stored");
        }

        return minX;
    }

    public void iterate(Triconsumer consumer) {
        for (int y = getMinY(); y <= getMaxY(); y++) {
            for (int x = getMinX(); x <= getMaxX(); x++) {
                String value = get(x, y);
                consumer.apply(x, y, value);
            }
        }
    }

    @FunctionalInterface
    public interface Triconsumer {
        void apply(int x, int y, String value);
    }

}
