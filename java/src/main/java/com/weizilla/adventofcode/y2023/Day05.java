package com.weizilla.adventofcode.y2023;

import com.weizilla.adventofcode.utils.Day;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

public class Day05 extends Day {
    public Day05(Integer example) {
        super(example);
    }

    @Override
    public Object part1() {
        var lines = reader.readStrings();

        var startSeeds = Arrays.stream(lines.get(0).split(":")[1].trim().split(" "))
            .map(Long::valueOf)
            .toList();

//        print("{}", startSeeds);

        var catMaps = parseMaps(lines);
//        print("{}", catMaps);

        Cat currCat = Cat.SEED;
        var currValues = new ArrayList<Long>(startSeeds);
        while (currCat != Cat.LOCATION) {
            var catMap = catMaps.get(currCat);

            var nextValues = new ArrayList<Long>();
            for (long currValue : currValues) {
                nextValues.add(catMap.getDestination(currValue));
            }
            currValues = nextValues;
            currCat = currCat.getDestination();
        }

        return currValues.stream().min(Long::compareTo).get();
    }

    private Map<Cat, CatMap> parseMaps(List<String> lines) {

        var maps = new HashMap<Cat, CatMap>();
        CatMap curr = null;
        for (int i = 1; i < lines.size(); i++) {
            String line = lines.get(i);
            if (line.isEmpty()) {
                continue;
            }

            if (line.contains("map")) {
                curr = new CatMap();
                var type = Cat.valueOf(line.split("-")[0].toUpperCase());
                maps.put(type, curr);
            } else {
                var splits = line.split(" ");
                curr.addRange(
                    Long.parseLong(splits[0]),
                    Long.parseLong(splits[1]),
                    Integer.parseInt(splits[2])
                );
            }
        }

        return maps;
    }

    @Override
    public Object part2() {
        var lines = reader.readStrings();

        var startValues = Arrays.stream(lines.get(0).split(":")[1].trim().split(" "))
            .map(Long::valueOf)
            .toList();

        var startRanges = new HashMap<Long, Long>();
        for (int i = 0; i < startValues.size(); i += 2) {
            startRanges.put(startValues.get(i), startValues.get(i + 1));
        }

        print("{}", startRanges);

        var catMaps = parseMaps(lines);
        print("{}", catMaps);

        Cat currCat = Cat.SEED;
        var currValues = new ArrayList<Long>();

        CatMap merged = null;
//        for (var e : startRanges.entrySet()) {
//            merged.addRange(e.getKey(), e.getKey(), e.getValue());
//        }

        while (currCat != Cat.LOCATION) {
            var catMap = catMaps.get(currCat);

            if (merged == null) {
                merged = catMap;
                continue;
            } else {
                merged = merge(merged, catMap);
            }

            currCat = currCat.getDestination();
        }

        print(merged);

//        return currValues.stream().min(Long::compareTo).get();
        return 0;

    }

    private CatMap merge(CatMap firstCatMap, CatMap secondCatMap) {
        var result = new CatMap();
        /*
          S  D
          ----    S   D
          first   ----
          ----    second
                  ----
         */

        for (Mapping first : firstCatMap.getMappings().sequencedValues()) {
            for (Mapping second : secondCatMap.getMappings().sequencedValues()) {
                if (first.destEnd() < second.source() || first.destination() > second.sourceEnd())  {
                    // no overlap
                    continue;
                }

                long midSource;
                long midDest;
                if (first.destination() < second.source()) {
                    /* S  D
                      ----    S   D
                      first   ----
                              second */
                    long len = second.source() - first.destination();
                    result.addRange(first.destination(), first.source(), len);
                    midSource = first.source() + len;
                    midDest = second.destination();
                } else if (first.destination() > second.source()){
                    /*        S   D
                      S  D    ------
                      ----    second
                      first
                              */
                    long len = first.destination() - second.source();
                    result.addRange(second.destination(), second.source(), len);
                    midSource = first.source();
                    midDest = second.destination() + len;
                } else {
                    midSource = first.source();
                    midDest = first.destination();
                }

                long midLen;
                if (first.destEnd() < second.sourceEnd()) {
                    /*
                      first
                      ----    second
                      S  D    -----
                              S   D
                     */
                    long len = second.sourceEnd() - first.destEnd();
                    result.addRange(second.destination() - len, second.source() - len, len);
                    midLen = midSource - first.source();
                } else if (first.destEnd() > second.sourceEnd()) {
                    /*        second
                      first   ------
                      ----    S    D
                      S  D
                     */
                    long len = first.destEnd() - second.sourceEnd();
                    result.addRange(first.destEnd() - len, first.sourceEnd() - len, len);
                    midLen = midDest - second.destination();
                } else {
                    midLen = first.length();
                }

                result.addRange(midDest, midSource, midLen);
            }
        }

        return result;
    }

    private enum Cat {
        LOCATION(null),
        HUMIDITY(LOCATION),
        TEMPERATURE(HUMIDITY),
        LIGHT(TEMPERATURE),
        WATER(LIGHT),
        FERTILIZER(WATER),
        SOIL(FERTILIZER),
        SEED(SOIL);

        private final Cat destination;

        Cat(Cat destionation) {
            this.destination = destionation;
        }

        public Cat getDestination() {
            return destination;
        }
    }

    private static class CatMap {
        private final NavigableMap<Long, Mapping> mappings;

        public CatMap() {
            mappings = new TreeMap<>();
        }

        public void addRange(long destination, long source, long length) {
            mappings.put(source, new Mapping(source, destination, length));
        }

        public long getDestination(long source) {
            var entry = mappings.floorEntry(source);
            if (entry == null) {
                return source;
            }
            var start = entry.getKey();
            var length = entry.getValue().length();
            var destination = entry.getValue().destination();
            if (start + length < source) {
                return source;
            }
            return destination + source - start;
        }

        public NavigableMap<Long, Mapping> getMappings() {
            return mappings;
        }


        @Override
        public String toString() {
            return "CatMap{" +
                "mappings=" + mappings +
                '}';
        }
    }

    private record Mapping(long source, long destination, long length) {
        public long sourceEnd() {
            return source + length;
        }

        public long destEnd() {
            return destination + length;
        }
    }

}
