package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.Song;

import java.util.List;

@Dao
public interface SongDao {

    @Insert
    void insert(Song song);

    @Update
    void update(Song song);

    @Delete
    void delete(Song song);

    @Query("SELECT * FROM Songs WHERE songID=:songID")
    List<Song> getSong(int songID);

    @Query("SELECT * FROM Songs WHERE userID=:userID AND songName=:songName")
    List<Song> findSong(int userID, String songName);

    @Query("SELECT * FROM Songs WHERE userID=:userID")
    LiveData<List<Song>> getAllSongs(int userID);
}
