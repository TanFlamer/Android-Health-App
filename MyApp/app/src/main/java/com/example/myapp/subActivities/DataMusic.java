package com.example.myapp.subActivities;

import android.graphics.Color;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.viewModal.DataMusicViewModel;
import com.google.android.material.textfield.TextInputLayout;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class DataMusic extends AppCompatActivity {

    DataMusicViewModel dataMusicViewModel;
    ListView songSelected, songUnselected;
    Button editSaveButton, returnButton;
    TextInputLayout playlistNameInput;
    ImageView addImageView, removeImageView;
    EditText playlistName;

    private final int ADD_TO_PLAYLIST = 1;
    private final int REMOVE_FROM_PLAYLIST = -1;
    HashMap<Integer, Integer> changeLogs;

    MusicDataListAdapter songUnselectedAdapter;
    MusicDataListAdapter songSelectedAdapter;

    List<Song> unselectedSongList;
    List<Song> selectedSongList;

    boolean[] unselectedArray;
    boolean[] selectedArray;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_music);
        dataMusicViewModel = new ViewModelProvider(this).get(DataMusicViewModel.class);
        //get playlist id from intent first
        Pair<List<Song>, List<Song>> songLists = dataMusicViewModel.populateLists();
        unselectedSongList = songLists.first;
        selectedSongList = songLists.second;
        changeLogs = new HashMap<>();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void initialiseAll(){
        initialiseListViews();
        initialiseEditText();
        initialiseImageView();
        initialiseButtons();
    }

    public void resetLists(){
        resetAdapters();
        resetImageViews();
        resetArrays();
    }

    public void resetArrays(){
        unselectedArray = new boolean[unselectedSongList.size()];
        selectedArray = new boolean[selectedSongList.size()];
    }

    public void resetImageViews(){
        addImageView.setAlpha((float) 0.35);
        addImageView.setClickable(false);
        removeImageView.setAlpha((float) 0.35);
        removeImageView.setClickable(false);
    }

    public void resetAdapters(){
        songUnselectedAdapter.notifyDataSetChanged();
        songSelectedAdapter.notifyDataSetChanged();
    }

    public boolean selected(boolean[] booleans){
        for(boolean bool : booleans)
            if(bool)
                return true;
        return false;
    }

    public void initialiseListViews(){
        songUnselected = findViewById(R.id.songUnselected);
        songUnselected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        songUnselected.setOnItemClickListener(onItemClickListener);

        songSelected = findViewById(R.id.songSelected);
        songSelected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        songSelected.setOnItemClickListener(onItemClickListener);

        songUnselectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, unselectedSongList);
        songUnselected.setAdapter(songUnselectedAdapter);

        songSelectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, selectedSongList);
        songSelected.setAdapter(songSelectedAdapter);
    }

    public void moveSongs(boolean[] booleans, List<Song> songListFrom, List<Song> songListTo, int operation){
        for(int i = booleans.length - 1; i >= 0; i--){
            if(booleans[i]){
                Song song = songListFrom.get(i);
                songListFrom.remove(i);
                songListTo.add(song);
                changeLogs.put(song.getSongID(), Objects.requireNonNull(changeLogs.getOrDefault(song.getSongID(), 0)) + operation);
            }
        }
    }

    public void initialiseImageView(){
        addImageView = findViewById(R.id.addImageView);
        addImageView.setOnClickListener(v -> {
            moveSongs(unselectedArray, unselectedSongList, selectedSongList, ADD_TO_PLAYLIST);
            resetLists();
        });

        removeImageView = findViewById(R.id.removeImageView);
        removeImageView.setOnClickListener(v -> {
            moveSongs(selectedArray, selectedSongList, unselectedSongList, REMOVE_FROM_PLAYLIST);
            resetLists();
        });

        resetLists();
    }

    public boolean updateImageView(boolean[] booleans, int position, ImageView imageView){
        booleans[position] = !booleans[position];
        boolean selected = selected(booleans);
        imageView.setAlpha((float)(selected ? 1 : 0.35));
        imageView.setClickable(selected);
        return booleans[position];
    }

    public void initialiseEditText(){
        playlistNameInput = findViewById(R.id.playlistNameInput);
        playlistName = findViewById(R.id.playlistName);
        playlistName.addTextChangedListener(playlistNameTextWatcher);
        playlistName.setOnFocusChangeListener((v, hasFocus) -> validatePlaylistName(playlistNameInput, playlistName));
    }

    public void initialiseButtons(){
        editSaveButton = findViewById(R.id.editSaveButton);
        returnButton = findViewById(R.id.returnButton);
        returnButton.setOnClickListener(v -> finish());
    }

    public boolean validatePlaylistName(TextInputLayout textInputLayout, EditText editText){
        String playlistText = editText.getText().toString();
        String oldPlaylistName = dataMusicViewModel.getPlaylistName();

        boolean hasFocus = editText.hasFocus();
        boolean emptyPlaylistName = playlistText.isEmpty();
        boolean validPlaylistName = !emptyPlaylistName && dataMusicViewModel.validatePlaylistName(playlistText);
        boolean equalPlaylistName = oldPlaylistName == null || playlistText.equals(oldPlaylistName);

        if(!hasFocus || equalPlaylistName || validPlaylistName)
            textInputLayout.setErrorEnabled(false);
        else if(emptyPlaylistName)
            textInputLayout.setError("Playlist name cannot be empty");
        else
            textInputLayout.setError("Playlist name already taken");
        return equalPlaylistName || validPlaylistName;
    }

    private final TextWatcher playlistNameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            boolean validPlaylistName = validatePlaylistName(playlistNameInput, playlistName);
            editSaveButton.setEnabled(validPlaylistName);
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    public AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        boolean selected = parent.equals(songUnselected) ? updateImageView(unselectedArray, position, addImageView) : updateImageView(selectedArray, position, removeImageView);
        view.setBackgroundColor(selected ? Color.BLUE : Color.WHITE);
        Song song = (Song) parent.getItemAtPosition(position);
        Toast.makeText(getApplicationContext(), song.getSongName() + " clicked", Toast.LENGTH_SHORT).show();
    };

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