package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import android.os.Build;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.DateConverter;
import com.example.test.databaseFiles.converter.DurationConverter;
import com.example.test.databaseFiles.converter.TimeConverter;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalTime;

@Entity(tableName = "Sleep",
        indices = @Index("userID"),
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Sleep {

    @PrimaryKey(autoGenerate = true)
    private Integer sleepID;

    @TypeConverters(DateConverter.class)
    private LocalDate date;

    @TypeConverters(TimeConverter.class)
    private LocalTime sleepTime;

    @TypeConverters(TimeConverter.class)
    private LocalTime wakeTime;

    @TypeConverters(DurationConverter.class)
    private Duration sleepDuration;

    private Integer userID;

    public Sleep(LocalDate date, LocalTime sleepTime, LocalTime wakeTime, Integer userID) {
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            this.sleepDuration = Duration.between(sleepTime, wakeTime);
        }
        this.userID = userID;
    }

    public Integer getSleepID() {
        return sleepID;
    }

    public void setSleepID(Integer sleepID) {
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

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
