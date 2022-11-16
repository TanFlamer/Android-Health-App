package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.DateConverter;
import com.example.test.databaseFiles.converter.IDConverter;

import java.time.LocalDate;
import java.util.List;

@Entity(tableName = "Sport",
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "user-id", onDelete = CASCADE))
public class Sport {
    @PrimaryKey(autoGenerate = true)
    private Integer sportID;

    @TypeConverters(DateConverter.class)
    @ColumnInfo(name = "date")
    private LocalDate date;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "type-id-list")
    private List<Integer> typeIDList;

    @ColumnInfo(name = "user-id", index = true)
    private Integer userID;


    public Sport(LocalDate date, List<Integer> typeIDList, Integer userID) {
        this.date = date;
        this.typeIDList = typeIDList;
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

    public List<Integer> getTypeIDList() {
        return typeIDList;
    }

    public void setTypeIDList(List<Integer> typeIDList) {
        this.typeIDList = typeIDList;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
