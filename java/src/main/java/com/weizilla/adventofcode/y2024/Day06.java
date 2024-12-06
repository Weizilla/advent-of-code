package com.weizilla.adventofcode.y2024;

import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.Matrix;
import com.weizilla.adventofcode.utils.Matrix.Entry;
import com.weizilla.adventofcode.utils.Point;
import org.checkerframework.checker.units.qual.N;

import java.util.HashSet;
import java.util.Set;

public class Day06 extends Day {
    public Day06(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        Matrix grid = reader.readMatrix();

        Entry starting = grid.findFirst((x, y, value) -> value.equals("^")).get();

        int x = starting.x();
        int y = starting.y();
        Direction dir = Direction.N;

        Set<Point> visited = new HashSet<>();

        while (x >= 0 && y >= 0 && x < grid.getMaxX() && y < grid.getMaxY()) {
            switch (dir) {
                case N -> y -= 1;
                case S -> y += 1;
                case E -> x += 1;
                case W -> x -= 1;
            }

            visited.add(new Point(x, y));

            if (frontIsBlocked(x, y, dir, grid)) {
                switch (dir) {
                    case N -> dir = Direction.E;
                    case E -> dir = Direction.S;
                    case S -> dir = Direction.W;
                    case W -> dir = Direction.N;
                }
            }

        }

        return visited.size();
    }

    @Override
    public Object part2() {
        Matrix grid = reader.readMatrix();

        Entry starting = grid.findFirst((x, y, value) -> value.equals("^")).get();

        int x = starting.x();
        int y = starting.y();
        Direction dir = Direction.N;

        Set<Visit> visited = new HashSet<>();

        while (x >= 0 && y >= 0 && x < grid.getMaxX() && y < grid.getMaxY()) {
            switch (dir) {
                case N -> y -= 1;
                case S -> y += 1;
                case E -> x += 1;
                case W -> x -= 1;
            }

            visited.add(new Visit(x, y, dir));

            Point right = getRight(x, y, dir);
            if (visited.contains(new Visit(right.x(), right.y(), dir))) {
                Point front =
            }

            if (isFrontBlocked(x, y, dir, grid)) {
                switch (dir) {
                    case N -> dir = Direction.E;
                    case E -> dir = Direction.S;
                    case S -> dir = Direction.W;
                    case W -> dir = Direction.N;
                }
            }

        }

        return visited.size();
    }

    private record Visit(int x, int y, Direction dir) { }

    private boolean isFrontBlocked(int x, int y, Direction dir, Matrix grid) {
        return "#".equals(grid.get(getFront(x, y, dir))) || "O".equals(grid.get(getFront(x, y, dir)));
    }

    private Point getFront(int x, int y, Direction dir) {
        return switch (dir) {
            case N -> new Point(x, y - 1);
            case E -> new Point(x + 1, y);
            case S -> new Point(x, y + 1);
            case W -> new Point(x - 1, y);
        };
    }

    private Point getRight(int x, int y, Direction dir) {
        return switch (dir) {
            case N -> new Point(x + 1, y);
            case E -> new Point(x, y + 1);
            case S -> new Point(x - 1, y);
            case W -> new Point(x, y - 1);
        };
    }

    private enum Direction {N, E, S, W}
}
