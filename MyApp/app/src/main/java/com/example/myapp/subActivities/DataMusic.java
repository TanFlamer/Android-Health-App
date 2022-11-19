package com.example.myapp.subActivities;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;

import com.example.myapp.R;
import com.example.myapp.subActivities.listMusicData.MusicDataListAdapter;
import com.example.myapp.subActivities.listMusicData.MusicDataListItem;

import java.util.ArrayList;
import java.util.List;

public class DataMusic extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_music);

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        ListView songUnselected = findViewById(R.id.songUnselected);
        ListView songSelected = findViewById(R.id.songSelected);

        List<MusicDataListItem> songUnselectedList = new ArrayList<>();
        List<MusicDataListItem> songSelectedList = new ArrayList<>();

        songUnselectedList.add(new MusicDataListItem("test", 0));
        songUnselectedList.add(new MusicDataListItem("test1", 1));

        songSelectedList.add(new MusicDataListItem("test", 0));
        songSelectedList.add(new MusicDataListItem("test1", 1));

        MusicDataListAdapter songUnselectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, songUnselectedList);
        songUnselected.setAdapter(songUnselectedAdapter);

        MusicDataListAdapter songSelectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, songSelectedList);
        songSelected.setAdapter(songSelectedAdapter);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}