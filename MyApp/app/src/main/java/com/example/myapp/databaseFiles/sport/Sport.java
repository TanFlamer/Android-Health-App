package com.example.myapp.databasefiles.sport;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databasefiles.user.User;

@Entity(tableName = "Sport",
        indices = { @Index("userID"), @Index("date") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Sport {

    @PrimaryKey(autoGenerate = true)
    private Integer sportID; //unique ID for each sport data regardless of user

    private Long date; //unique date for each sport data (same date allowed for different user)

    private Integer userID; //user ID of the user who the sport data belongs to

    //constructor for new sport data
    public Sport(Long date, Integer userID) {
        this.date = date;
        this.userID = userID;
    }

    @Ignore //constructor to change sport data
    public Sport(Integer sportID, Long date, Integer userID) {
        this.sportID = sportID;
        this.date = date;
        this.userID = userID;
    }

    //getter for sport data ID
    public Integer getSportID() {
        return sportID;
    }

    //setter for sport data ID
    public void setSportID(Integer sportID) {
        this.sportID = sportID;
    }

    //getter for sport date
    public Long getDate() {
        return date;
    }

    //setter for sport date
    public void setDate(Long date) {
        this.date = date;
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
