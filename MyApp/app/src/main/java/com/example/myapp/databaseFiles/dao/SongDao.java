package com.example.myapp.databaseFiles.dao;

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
    Song findSong(int songID);

    @Query("SELECT * FROM Songs")
    List<Song> getAllSongs();
}
