package com.example.myapp.subActivities;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;

import androidx.appcompat.app.AppCompatActivity;

import com.example.myapp.R;
import com.example.myapp.subActivities.listSportData.SportDataListAdapter;
import com.example.myapp.subActivities.listSportData.SportDataListItem;

import java.util.ArrayList;
import java.util.List;

public class DataSport extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);

        ListView listView = findViewById(R.id.sportDataListView);
        List<SportDataListItem> sportDataListItemList = new ArrayList<>();

        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));
        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));
        sportDataListItemList.add(new SportDataListItem("test", 0, 0));
        sportDataListItemList.add(new SportDataListItem("test1", 1, 1));

        SportDataListAdapter sportDataListAdapter = new SportDataListAdapter(this, R.layout.data_sport_list_item, sportDataListItemList);
        listView.setAdapter(sportDataListAdapter);

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
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