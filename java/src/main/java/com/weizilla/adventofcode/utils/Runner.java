package com.weizilla.adventofcode.utils;

import com.google.common.reflect.ClassPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Method;
import java.util.Set;
import java.util.stream.Collectors;

public class Runner {
    private static final Logger logger = LoggerFactory.getLogger(Runner.class);


    private static Set<Class> getAllClasses() throws Exception {
        return ClassPath.from(ClassLoader.getSystemClassLoader())
            .getAllClasses()
            .stream()
                .filter(c -> c.getPackageName().contains("com.weizilla.adventofcode.y"))
            .map(ClassPath.ClassInfo::load)
            .collect(Collectors.toSet());
    }

    public static void main(String[] args) {
        try {
            Set<Class> classes = getAllClasses();
            //TODO
            Class next = classes.iterator().next();
            Object i = next.getConstructors()[0].newInstance();
            Method part1 = Day.class.getMethod("part1");
            Object invoke = part1.invoke(i);
            logger.info("Result {}", invoke);
        } catch (Exception e) {
            logger.error("error", e);
        }
    }
}
