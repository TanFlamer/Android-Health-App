package com.example.myapp.databaseFiles.sport;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databaseFiles.user.User;

@Entity(tableName = "Sport",
        indices = { @Index("userID"), @Index("date") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Sport {

    @PrimaryKey(autoGenerate = true)
    private Integer sportID;

    private Long date;

    private Integer userID;

    public Sport(Long date, Integer userID) {
        this.date = date;
        this.userID = userID;
    }

    @Ignore
    public Sport(Integer sportID, Long date, Integer userID) {
        this.sportID = sportID;
        this.date = date;
        this.userID = userID;
    }

    public Integer getSportID() {
        return sportID;
    }

    public void setSportID(Integer sportID) {
        this.sportID = sportID;
    }

    public Long getDate() {
        return date;
    }

    public void setDate(Long date) {
        this.date = date;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
