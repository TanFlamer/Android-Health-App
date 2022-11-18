package com.example.test.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.TypeConverters;

import com.example.test.databaseFiles.converter.DurationConverter;

import java.time.Duration;

@Entity(tableName = "TypeSport",
        primaryKeys = { "sportID", "typeID" },
        indices = { @Index("sportID"), @Index("typeID") },
        foreignKeys = { @ForeignKey(entity = Sport.class, parentColumns = "sportID", childColumns = "sportID", onDelete = CASCADE),
                        @ForeignKey(entity = Type.class, parentColumns = "typeID", childColumns = "typeID", onDelete = CASCADE) })
public class TypeSport {

    @NonNull
    private Integer sportID;

    @NonNull
    private Integer typeID;

    @TypeConverters(DurationConverter.class)
    private Duration duration;

    public TypeSport(@NonNull Integer sportID, @NonNull Integer typeID, Duration duration){
        this.sportID = sportID;
        this.typeID = typeID;
        this.duration = duration;
    }

    @NonNull
    public Integer getSportID() {
        return sportID;
    }

    public void setSportID(@NonNull Integer sportID) {
        this.sportID = sportID;
    }

    @NonNull
    public Integer getTypeID() {
        return typeID;
    }

    public void setTypeID(@NonNull Integer typeID) {
        this.typeID = typeID;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }
}