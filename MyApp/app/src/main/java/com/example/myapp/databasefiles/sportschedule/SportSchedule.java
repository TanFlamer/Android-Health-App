package com.example.myapp.databaseFiles.sportschedule;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

import com.example.myapp.databaseFiles.sport.Sport;
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.databaseFiles.user.User;

@Entity(tableName = "SportSchedule",
        primaryKeys = { "sportID", "typeID" },
        indices = { @Index("sportID"), @Index("typeID"), @Index("userID") },
        foreignKeys = { @ForeignKey(entity = Sport.class, parentColumns = "sportID", childColumns = "sportID", onDelete = CASCADE),
                        @ForeignKey(entity = Type.class, parentColumns = "typeID", childColumns = "typeID", onDelete = CASCADE),
                        @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE) })
public class SportSchedule {

    @NonNull
    private final Integer sportID; //sport ID of a specific sport data

    @NonNull
    private final Integer typeID; //type ID of a specific sport type

    private final Integer sportDuration; //duration of the sport type

    @NonNull
    private final Integer userID; //user ID of the user who the sport schedule belongs to

    //constructor for new sport schedule
    public SportSchedule(@NonNull Integer sportID, @NonNull Integer typeID, Integer sportDuration, @NonNull Integer userID){
        this.sportID = sportID;
        this.typeID = typeID;
        this.sportDuration = sportDuration;
        this.userID = userID;
    }

    @NonNull //getter for sport data ID
    public Integer getSportID() {
        return sportID;
    }

    @NonNull //getter for sport type ID
    public Integer getTypeID() {
        return typeID;
    }

    //getter for sport duration
    public Integer getSportDuration() {
        return sportDuration;
    }

    @NonNull //getter for user ID
    public Integer getUserID() {
        return userID;
    }
}
