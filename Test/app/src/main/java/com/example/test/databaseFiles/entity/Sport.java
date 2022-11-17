package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.DateConverter;
import com.example.test.databaseFiles.converter.IDConverter;

import java.time.LocalDate;
import java.util.List;

@Entity(tableName = "Sport",
        indices = @Index("userID"),
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Sport {

    @PrimaryKey(autoGenerate = true)
    private Integer sportID;

    @TypeConverters(DateConverter.class)
    private LocalDate date;

    private Integer userID;

    public Sport(LocalDate date, Integer userID) {
        this.date = date;
        this.userID = userID;
    }

    public Integer getSportID() {
        return sportID;
    }

    public void setSportID(Integer sportID) {
        this.sportID = sportID;
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
