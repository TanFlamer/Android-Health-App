package com.example.myapp.databaseFiles.entity;

import android.os.Build;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.DateConverter;
import com.example.myapp.databaseFiles.converter.TimeConverter;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;

@Entity(tableName = "Sleep")
public class Sleep {
    @PrimaryKey(autoGenerate = true)
    private int sleepID;

    @TypeConverters(DateConverter.class)
    @ColumnInfo(name = "date")
    private LocalDate date;

    @TypeConverters(TimeConverter.class)
    @ColumnInfo(name = "sleep-time")
    private LocalTime sleepTime;

    @TypeConverters(TimeConverter.class)
    @ColumnInfo(name = "wake-time")
    private LocalTime wakeTime;

    @TypeConverters(TimeConverter.class)
    @ColumnInfo(name = "sleep-duration")
    private Duration sleepDuration;

    public Sleep(LocalDate date, LocalTime sleepTime, LocalTime wakeTime) {
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            this.sleepDuration = Duration.between(sleepTime, wakeTime);
        }
    }

    public int getSleepID() {
        return sleepID;
    }

    public void setSleepID(int sleepID) {
        this.sleepID = sleepID;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public LocalTime getSleepTime() {
        return sleepTime;
    }

    public void setSleepTime(LocalTime sleepTime) {
        this.sleepTime = sleepTime;
    }

    public LocalTime getWakeTime() {
        return wakeTime;
    }

    public void setWakeTime(LocalTime wakeTime) {
        this.wakeTime = wakeTime;
    }

    public Duration getSleepDuration() {
        return sleepDuration;
    }

    public void setSleepDuration(Duration sleepDuration) {
        this.sleepDuration = sleepDuration;
    }
}
