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
    private Integer sleepID; //unique ID for each sleep data regardless of user

    private Long date; //unique date for each sleep data (same date allowed for different user)

    private Integer sleepTime; //sleep time for each sleep data

    private Integer wakeTime; //wake time for each sleep data

    private Integer userID; //user ID of the user who the sleep data belongs to

    //constructor for new sleep data
    public Sleep(Long date, Integer sleepTime, Integer wakeTime, Integer userID) {
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.userID = userID;
    }

    @Ignore //constructor to change sleep data
    public Sleep(Integer sleepID, Long date, Integer sleepTime, Integer wakeTime, Integer userID) {
        this.sleepID = sleepID;
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.userID = userID;
    }

    //getter for sleep data ID
    public Integer getSleepID() {
        return sleepID;
    }

    //setter for sleep data ID
    public void setSleepID(Integer sleepID) {
        this.sleepID = sleepID;
    }

    //getter for sleep date
    public Long getDate() {
        return date;
    }

    //setter for sleep date
    public void setDate(Long date) {
        this.date = date;
    }

    //getter for sleep time
    public Integer getSleepTime() {
        return sleepTime;
    }

    //setter for sleep time
    public void setSleepTime(Integer sleepTime) {
        this.sleepTime = sleepTime;
    }

    //getter for wake time
    public Integer getWakeTime() {
        return wakeTime;
    }

    //setter for wake time
    public void setWakeTime(Integer wakeTime) {
        this.wakeTime = wakeTime;
    }

    //getter for user ID
    public Integer getUserID() {
        return userID;
    }

    //setter for user ID
    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
