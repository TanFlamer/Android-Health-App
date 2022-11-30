package com.example.myapp.databasefiles.sleep;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databasefiles.user.User;

@Entity(tableName = "Sleep",
        indices = { @Index("userID"), @Index("date") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Sleep {

    @PrimaryKey(autoGenerate = true)
    private Integer sleepID;

    private Long date;

    private Integer sleepTime;

    private Integer wakeTime;

    private Integer userID;

    public Sleep(Long date, Integer sleepTime, Integer wakeTime, Integer userID) {
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.userID = userID;
    }

    @Ignore
    public Sleep(Integer sleepID, Long date, Integer sleepTime, Integer wakeTime, Integer userID) {
        this.sleepID = sleepID;
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.userID = userID;
    }

    public Integer getSleepID() {
        return sleepID;
    }

    public void setSleepID(Integer sleepID) {
        this.sleepID = sleepID;
    }

    public Long getDate() {
        return date;
    }

    public void setDate(Long date) {
        this.date = date;
    }

    public Integer getSleepTime() {
        return sleepTime;
    }

    public void setSleepTime(Integer sleepTime) {
        this.sleepTime = sleepTime;
    }

    public Integer getWakeTime() {
        return wakeTime;
    }

    public void setWakeTime(Integer wakeTime) {
        this.wakeTime = wakeTime;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
