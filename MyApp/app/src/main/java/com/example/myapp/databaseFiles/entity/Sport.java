package com.example.myapp.databaseFiles.entity;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.example.myapp.databaseFiles.converter.DateConverter;
import com.example.myapp.databaseFiles.converter.IDConverter;

import java.time.LocalDate;
import java.util.List;

@Entity(tableName = "Sport")
public class Sport {
    @PrimaryKey(autoGenerate = true)
    private int sportID;

    @TypeConverters(DateConverter.class)
    @ColumnInfo(name = "date")
    private LocalDate date;

    @TypeConverters(IDConverter.class)
    @ColumnInfo(name = "type-id-list")
    private List<Integer> typeIDList;

    public Sport(LocalDate date, List<Integer> typeIDList) {
        this.date = date;
        this.typeIDList = typeIDList;
    }

    public int getSportID() {
        return sportID;
    }

    public void setSportID(int sportID) {
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
}
