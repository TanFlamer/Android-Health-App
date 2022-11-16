package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.DurationConverter;

import java.time.Duration;

@Entity(tableName = "Types",
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "user-id", onDelete = CASCADE))
public class Type {
    @PrimaryKey(autoGenerate = true)
    private Integer typeID;

    @ColumnInfo(name = "name")
    private String name;

    @TypeConverters(DurationConverter.class)
    @ColumnInfo(name = "duration")
    private Duration duration;

    @ColumnInfo(name = "calorie-per-minute")
    private Double caloriePerMinute;

    @ColumnInfo(name = "user-id", index = true)
    private Integer userID;

    public Type(String name, Duration duration, Double caloriePerMinute, Integer userID) {
        this.name = name;
        this.duration = duration;
        this.caloriePerMinute = caloriePerMinute;
        this.userID = userID;
    }

    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(Integer typeID) {
        this.typeID = typeID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }

    public Double getCaloriePerMinute() {
        return caloriePerMinute;
    }

    public void setCaloriePerMinute(Double caloriePerMinute) {
        this.caloriePerMinute = caloriePerMinute;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
