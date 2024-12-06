package com.weizilla.adventofcode.y2024;

import com.weizilla.adventofcode.utils.Day;
import com.weizilla.adventofcode.utils.Matrix;
import com.weizilla.adventofcode.utils.Matrix.Entry;
import com.weizilla.adventofcode.utils.Point;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Day06 extends Day {
    private static final Map<Direction, String> CURR = Map.of(
        Direction.N, "^", Direction.E, ">", Direction.S, "V", Direction.W, "<"
    );

    private static final Map<Direction, String> OLD = Map.of(
        Direction.N, "|", Direction.E, "-", Direction.S, "|", Direction.W, "-"
    );

    public Day06(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        Matrix grid = reader.readMatrix();

        Entry starting = grid.findFirst((x, y, value) -> value.equals("^")).get();

        Visit curr = new Visit(starting.x(), starting.y(), Direction.N);

        Set<Point> visited = new HashSet<>();

        int step = 0;
        while (grid.isInBounds(curr.point)) {
            grid.update(curr.point, OLD.get(curr.dir));
            visited.add(curr.point);

            if (isFrontBlocked(curr, grid)) {
                curr = getRight(curr);
            } else {
                curr = getFront(curr);
            }

            if (grid.isInBounds(curr.point)) {
                grid.update(curr.point, CURR.get(curr.dir));
            }
            print("{}\n{}", step, grid.prettyPrint());
            step++;
        }

        return visited.size();
    }

    private static Direction getRightDirection(Direction dir) {
        return switch (dir) {
            case N -> Direction.E;
            case E -> Direction.S;
            case S -> Direction.W;
            case W -> Direction.N;
        };
    }

    @Override
    public Object part2() {
        Matrix grid = reader.readMatrix();

        Entry starting = grid.findFirst((x, y, value) -> value.equals("^")).get();

        Visit curr = new Visit(starting.x(), starting.y(), Direction.N);

        Set<Visit> visited = new HashSet<>();

        Set<Visit> obstacles = new HashSet<>();

        int step = 0;
        while (grid.isInBounds(curr.point)) {
            grid.update(curr.point, OLD.get(curr.dir));
            visited.add(curr);

            if (checkLoop(curr, grid, visited)) {
                Visit front = getFront(curr);
                if (!"#".equals(grid.get(front.point))) {
                    grid.update(front.point, "O");
                    obstacles.add(front);
                    print("OOOOOOOOOOOO {}\n{}", step, grid.prettyPrint());
                }
            }

            if (isFrontBlocked(curr, grid)) {
                curr = getRight(curr);
            } else {
                curr = getFront(curr);
            }

            if (grid.isInBounds(curr.point)) {
                grid.update(curr.point, CURR.get(curr.dir));
            }
            print("{}\n{}", step, grid.prettyPrint());
            step++;
        }

        return obstacles.size();
    }

    private record Visit(Point point, Direction dir) {
        Visit(int x, int y, Direction dir) {
            this(new Point(x, y), dir);
        }
    }

    private boolean isFrontBlocked(Visit visit, Matrix grid) {
        Visit front = getFront(visit);
        return "#".equals(grid.get(front.point));
    }

    private Visit getFront(Visit visit) {
        int x = visit.point.x();
        int y = visit.point.y();
        Direction dir = visit.dir();
        return switch (dir) {
            case N -> new Visit(x, y - 1, dir);
            case E -> new Visit(x + 1, y, dir);
            case S -> new Visit(x, y + 1, dir);
            case W -> new Visit(x - 1, y, dir);
        };
    }

    private boolean checkLoop(Visit visit, Matrix grid, Set<Visit> visited) {
        Visit curr = getRight(visit);
        String value = grid.get(curr.point);
        while (!"#".equals(value) && grid.isInBounds(curr.point)) {
            boolean hitPath = visited.contains(curr);
            if (hitPath) {
                return true;
            }
            curr = getFront(curr);
            value = grid.get(curr.point);
        }
        return false;
    }

    private Visit getRight(Visit visit) {
        Direction dir = visit.dir;
        int x = visit.point.x();
        int y = visit.point.y();
        Direction r = getRightDirection(dir);
        return switch (dir) {
            case N -> new Visit(x + 1, y, r);
            case E -> new Visit(x, y + 1, r);
            case S -> new Visit(x - 1, y, r);
            case W -> new Visit(x, y - 1, r);
        };
    }

    private enum Direction {N, E, S, W}
}