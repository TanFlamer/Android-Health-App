package com.example.myapp.subActivities.sport;

import android.annotation.SuppressLint;
import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.Spinner;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;
import com.example.myapp.subActivities.type.TypeDataAdapter;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;

public class SportDataActivity extends AppCompatActivity {

    SportDataViewModel sportDataViewModel;
    LinearLayout hiddenLayout;
    ListView dataSportList;
    Button buttonDate, buttonAdd, buttonDuration, buttonDelete, buttonEditSave, buttonReturn;
    Spinner typeSpinner;

    SportDataListAdapter sportDataListAdapter;
    TypeDataAdapter spinnerAdapter;

    HashMap<Integer, Pair<Integer, Integer>> changeLogs;
    List<Pair<Pair<Type, Integer>, Boolean>> sportDataList;
    List<Type> typeList;

    int year, month, day;
    int hour, minute;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);
        //get view model
        sportDataViewModel = new ViewModelProvider(this).get(SportDataViewModel.class);
        //show back button on top
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        //initialise all components
        initialiseAll();
    }

    //initialise all components
    public void initialiseAll(){
        //link all components by ID
        findAllViewByID();
        //initialise all lists
        initialiseLists();
        //initialise sport list view
        initialiseListView();
        //initialise spinner for sport type
        initialiseSpinners();
        //initialise all buttons
        initialiseButtons();
    }

    //link all components by ID
    public void findAllViewByID(){
        hiddenLayout = findViewById(R.id.hiddenLayout);
        dataSportList = findViewById(R.id.sportDataListView);
        typeSpinner = findViewById(R.id.typeSpinner);
        buttonAdd = findViewById(R.id.buttonAdd);
        buttonDelete = findViewById(R.id.buttonDelete);
        buttonDate = findViewById(R.id.buttonDate);
        buttonDuration = findViewById(R.id.buttonDuration);
        buttonEditSave = findViewById(R.id.buttonEditSave);
        buttonReturn = findViewById(R.id.buttonReturn);
    }

    //initialise all lists
    public void initialiseLists(){
        changeLogs = new HashMap<>();
        sportDataList = new ArrayList<>();
        typeList = new ArrayList<>();
    }

    //initialise sport list view
    public void initialiseListView(){
        //set on item click listener
        dataSportList.setOnItemClickListener(onItemClickListener);
        //initialise new list adapter
        sportDataListAdapter = new SportDataListAdapter(this, sportDataList);
        //set new adapter for list view
        dataSportList.setAdapter(sportDataListAdapter);
    }

    //initialise spinner for sport type
    public void initialiseSpinners(){
        //set on item selected listener
        typeSpinner.setOnItemSelectedListener(onItemSelectedListener);
        //initialise new spinner list adapter
        spinnerAdapter = new TypeDataAdapter(this, android.R.layout.simple_spinner_dropdown_item, typeList);
        //set new adapter for spinner
        typeSpinner.setAdapter(spinnerAdapter);
    }

    //initialise all buttons
    public void initialiseButtons(){
        //initialise add button
        initialiseAddButton();
        //initialise delete button
        initialiseDeleteButton();
        //initialise date button
        initialiseDateButton();
        //initialise duration button
        initialiseDurationButton();
        //initialise save and return button
        initialiseBottomButtons();
    }

    //initialise add button
    public void initialiseAddButton(){
        //set on click listener
        buttonAdd.setOnClickListener(v -> {
            //add new sport type to list
            addSport();
            //refresh the list and spinner
            resetList();
        });
    }

    //initialise delete button
    public void initialiseDeleteButton(){
        //set on click listener
        buttonDelete.setOnClickListener(v -> {
            //add existing sport type from list
            deleteSports();
            //refresh the list and spinner
            resetList();
        });
    }

    //initialise date button
    @SuppressLint("DefaultLocale")
    public void initialiseDateButton(){
        //initialise calendar view with initial date
        initialiseDate();
        //show date on button
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        //set on click listener
        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            //set year, month and day
            year = i;
            month = i1 + 1;
            day = i2;
            //update activity with sport types from given date
            populateLists(LocalDate.of(year, month, day).atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli());
            //show new date on button
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        }, year, month - 1, day).show());
    }

    //initialise duration button
    @SuppressLint("DefaultLocale")
    public void initialiseDurationButton(){
        //set on click listener
        buttonDuration.setOnClickListener(v -> new TimePickerDialog(SportDataActivity.this, android.R.style.Theme_Holo_Light_Dialog, (timePicker, i, i2) -> {
            //set sport hour and minute
            hour = i;
            minute = i2;
            //show new sport duration on button
            buttonDuration.setText(hour == 0 && minute == 0 ? "Select Duration" : String.format("%d:%02d", hour, minute));
            //enable add button if valid sport duration
            checkAddButton();
        }, hour, minute, true).show());
    }

    //initialise save and return button
    @SuppressLint("SetTextI18n")
    public void initialiseBottomButtons(){
        //set save button on click listener
        buttonEditSave.setOnClickListener(v -> {
            //if layout hidden, show layout
            if(hiddenLayout.getVisibility() == View.GONE){
                buttonEditSave.setText("Save");
                buttonEditSave.setEnabled(false);
                hiddenLayout.setVisibility(View.VISIBLE);
            }
            else { //else save new sport data
                //get current local date
                long date = LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
                if(sportDataViewModel.getSport() == null) //insert new sport data if no date data
                    sportDataViewModel.insertSport(date);
                changeLogs.forEach((typeID, pair) -> { //loop through changes logs
                    int operation = pair.first;
                    int duration = pair.second;
                    if(operation > 0) //if new sport data
                        sportDataViewModel.insertTypeSport(typeID, duration); //insert new sport schedule
                    else if(operation < 0) //if sport data deleted
                        sportDataViewModel.deleteTypeSport(typeID, duration); //delete existing sport schedule
                    else //else if sport data changed
                        sportDataViewModel.updateTypeSport(typeID, duration);//update existing sport schedule
                });
                finish(); //return to last activity
            }
        });
        //set return button on click listener to return to last activity
        buttonReturn.setOnClickListener(v -> finish());
    }

    //initialise calendar view with initial date
    public void initialiseDate(){
        //get date from intent
        long dateMillis = getIntent().getExtras().getLong("date");
        LocalDate date = Instant.ofEpochMilli(dateMillis).atZone(ZoneId.systemDefault()).toLocalDate();
        //set year, month and day
        year = date.getYear();
        month = date.getMonthValue();
        day = date.getDayOfMonth();
        //update activity with sport types from given date
        populateLists(dateMillis);
    }

    //update activity with sport types from given date
    public void populateLists(long date){
        Pair<List<Pair<Type, Integer>>, List<Type>> listPair = sportDataViewModel.populateList(date);

        //clear old selected sport type list
        sportDataList.clear();
        //clear old unselected sport type list
        typeList.clear();

        //add new selected sport type list
        for(Pair<Type, Integer> typeDurationPair : listPair.first) sportDataList.add(new Pair<>(typeDurationPair, false));
        //add new unselected sport type list
        typeList.addAll(listPair.second);
        //refresh the list and spinner
        resetList();
    }

    //check if add button should be enabled
    public void checkAddButton(){
        //check if spinner is empty
        boolean emptySpinner = typeList.isEmpty();
        //check if sport duration is valid
        boolean validDuration = minute > 0 || hour > 0;
        //enable add button if spinner is not empty and sport duration is valid
        buttonAdd.setEnabled(!emptySpinner && validDuration);
    }

    @SuppressLint("SetTextI18n")
    public void addSport(){
        Type type = (Type) typeSpinner.getSelectedItem();
        int duration = hour * 60 + minute;
        //add sport type to selected list
        sportDataList.add(new Pair<>(new Pair<>(type, duration), false));
        //remove sport type from spinner list
        typeList.remove(type);
        //reset duration button
        buttonDuration.setText("Select Duration");
        hour = minute = 0;
        //modify change logs
        modifyChangeLogs(type.getTypeID(), duration, 1);
    }

    public void deleteSports(){
        for(int i = sportDataList.size() - 1; i >= 0; i--){
            Pair<Pair<Type, Integer>, Boolean> pairBooleanPair = sportDataList.get(i);
            if(pairBooleanPair.second){
                Pair<Type, Integer> typeDurationPair = pairBooleanPair.first;
                //remove sport type from selected list
                sportDataList.remove(i);
                Type type = typeDurationPair.first;
                //add sport type to spinner list
                typeList.add(type);
                //modify change logs
                modifyChangeLogs(type.getTypeID(), typeDurationPair.second, -1);
            }
        }
    }

    //modify change logs
    public void modifyChangeLogs(int typeID, int duration, int operation){
        Pair<Integer, Integer> pair = Objects.requireNonNull(changeLogs.getOrDefault(typeID, new Pair<>(0, 0)));
        if(pair.first == 0)
            changeLogs.put(typeID, new Pair<>(operation, duration));
        else{
            if(duration == pair.second)
                changeLogs.remove(typeID);
            else
                changeLogs.put(typeID, new Pair<>(0, duration));
        }
    }

    //refresh the list and spinner
    public void resetList(){
        //refresh list adapters for sport list and spinner
        resetAdapters();
        //reset edit, delete and add buttons
        resetButtons();
    }

    //refresh list adapters for sport list and spinner
    public void resetAdapters(){
        //notify sport list that dataset has changed
        sportDataListAdapter.notifyDataSetChanged();
        //notify spinner that dataset has changed
        spinnerAdapter.notifyDataSetChanged();
    }

    //reset edit, delete and add buttons
    public void resetButtons(){
        //enable save/edit button if sport list and change logs are not empty or layout is gone
        buttonEditSave.setEnabled(!(sportDataList.isEmpty() || changeLogs.isEmpty()) || hiddenLayout.getVisibility() == View.GONE);
        //enable delete button if any sport type is selected
        buttonDelete.setEnabled(selected());
        //check if add button should be enabled
        checkAddButton();
    }

    //check if any sport type from list selected
    public boolean selected(){
        for(Pair<Pair<Type, Integer>, Boolean> pair : sportDataList)
            if(pair.second) //if at least one sport type selected, return true
                return true;
        return false; //if no sport type selected, return false
    }

    //on item click listener for list view
    AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        Pair<Pair<Type, Integer>, Boolean> pairBooleanPair = sportDataList.get(position);
        //change song view colour depending on selected state
        view.setBackgroundColor(pairBooleanPair.second ? Color.WHITE : Color.LTGRAY);
        sportDataList.set(position, new Pair<>(pairBooleanPair.first, !pairBooleanPair.second));
        //enable delete button if any sport type is selected
        buttonDelete.setEnabled(selected());
    };

    //on item selected listener for spinner
    AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override //if item selected, check if add button should be enabled
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            checkAddButton();
        }

        @Override //if nothing, check if add button should be enabled
        public void onNothingSelected(AdapterView<?> parent) {
            checkAddButton();
        }
    };

    @Override //set back button on top to return to last activity
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    //initialise options menu
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}