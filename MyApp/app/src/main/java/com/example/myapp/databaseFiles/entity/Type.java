package com.example.myapp.databaseFiles.entity;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.DurationConverter;

import java.time.Duration;

@Entity(tableName = "Types")
public class Type {
    @PrimaryKey(autoGenerate = true)
    private int typeID;

    @ColumnInfo(name = "name")
    private String name;

    @TypeConverters(DurationConverter.class)
    @ColumnInfo(name = "duration")
    private Duration duration;

    @ColumnInfo(name = "calorie-per-minute")
    private double caloriePerMinute;

    public Type(String name, Duration duration, double caloriePerMinute) {
        this.name = name;
        this.duration = duration;
        this.caloriePerMinute = caloriePerMinute;
    }
}
